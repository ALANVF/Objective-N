package objn

import objn.Ast.{Context, Expr, Statement => Stmt}
import objn.Ast.Expr.FuncArgs.Args
import objn.Ast.Expr.FuncArgs.Varargs
import scala.collection.mutable.{Map => MutableMap}

// TODO: add support for classes and complex objects

object Checker {
	sealed trait Type {
		def name: String
		def |(other: Type) = reduceUnion(Set(this, other))
		def accepts(other: Type) = other == this
		def isSubtypeOf(other: Type) = other accepts this
		def isSameAs(other: Type) = this == other
		def toObjn(): String = this match {
			case TArray(t) => "$tarray(" + t.toObjn() + ')'
			case TFunction(params, ret) => "$tfunction(" + (params match {
				case Some(params2) => params2.map(_.toObjn()).mkString(", ")
				case None => "..."
			}) + "): " + ret.toObjn()
			case TAbstract(None) => "$tabstract"
			case TAbstract(Some(repr)) => "$tabstract("+repr+')'
			case TUnion(types) => types.map(_.toObjn()).mkString(" | ")
			case _ => this.name
		}
	}
	object Type {
		def fromExprType(etype: Expr.Type): Type = etype match {
			case Expr.Type.Dynamic => TUnknown
			case Expr.Type.Builtin(name) => name match {
				case "$tnull" => TNull
				case "$tint" => TInt
				case "$tfloat" => TFloat
				case "$tbool" => TBool
				case "$tstring" => TString
				case "$tobject" => TObject
				case "$tarray" => TArray()
				case "$tfunction" => TFunction()
				case "$tabstract" => TAbstract(None)
			}
			case Expr.Type.Compound(_) => TObject // fix once compound types are added to the checker
			case Expr.Type.Paren(t) => fromExprType(t)
			case Expr.Type.Union(types) => union(types.map(fromExprType) : _*)
			case Expr.Type.Array(t) => TArray(fromExprType(t))
			case Expr.Type.Func(params, ret) => TFunction(params.map(_.map(fromExprType)), fromExprType(ret))
			case Expr.Type.Abstract(name) => TAbstract(Some(name))
		}
		
		def reduce(typ: Type) = typ match {
			case TUnion(types) => reduceUnion(types)
			case TArray(TUnion(types)) => reduceUnion(types)
			case _ => typ
		}
		
		val tKind = TAbstract(Some("kind"))
		val tHash = TAbstract(Some("hash"))
		val tInt32 = TAbstract(Some("int32"))
		
		val anyInt = union(TInt, tInt32)
	}
	case object TNull extends Type { def name = "$tnull" }
	case object TInt extends Type { def name = "$tint" }
	case object TFloat extends Type { def name = "$tfloat" }
	case object TBool extends Type { def name = "$tbool" }
	case object TString extends Type { def name = "$tstring" }
	case object TObject extends Type { def name = "$tobject" }
	case class TArray(elem: Type = TUnknown) extends Type {
		def name = "$tarray"
		override def accepts(other: Type) = other match {
			case TArray(elem) => this.elem accepts elem
			case _ => false
		}
		override def isSameAs(other: Type) = other match {
			case TArray(elem) => this.elem isSameAs elem
			case _ => false
		}
	}
	case class TFunction(params: Option[List[Type]] = None, ret: Type = TUnknown) extends Type {
		def name = "$tfunction"
		override def accepts(other: Type) = other match {
			case TFunction(params, ret) => this.ret.accepts(ret) && ((this.params, params) match {
				case (None, _) => true
				case (Some(_), None) => false
				case (Some(p1), Some(p2)) => p1.corresponds(p2) {_ accepts _}
			})
			case _ => false
		}
		override def isSameAs(other: Type) = other match {
			case TFunction(params, ret) => this.ret.isSameAs(ret) && ((this.params, params) match {
				case (None, _) => true // should this actually fail? hmm...
				case (Some(_), None) => false
				case (Some(p1), Some(p2)) => p1.corresponds(p2) {_ isSameAs _}
			})
			case _ => false
		}
	}
	case class TAbstract(repr: Option[String]) extends Type {
		def name = "$tabstract"
		override def accepts(other: Type) = this isSameAs other
	}
	// Maybe add objn object types later...
	case object TUnknown extends Type {
		def name = "???"
		override def |(other: Type) = this
		override def accepts(other: Type) = true
		override def isSubtypeOf(other: Type) = other == this
	}
	case object TInvalid extends Type {
		def name = "!!!"
		override def |(other: Type) = this
		override def accepts(other: Type) = false
		override def isSubtypeOf(other: Type) = false
	}
	case class TUnion(types: Set[Type]) extends Type {
		//def this(types: Type*) = this(types.toSet)
		def name = types.map(_.name).mkString("(", " | ", ")")
		override def accepts(other: Type) = other match {
			case TUnknown | TInvalid => false
			case TUnion(types) => types.forall(this accepts _)
			case t => this.types.exists(_ accepts t)
		}
		override def isSameAs(other: Type) = other match {
			case TUnion(types) => this.types.corresponds(types) {_ isSameAs _} // meh probably wrong
			case _ => false
		}
		def unapply(t: Type) = this accepts t
	}
	
	def typeFuncParams(ctx: Context, params: Expr.FuncArgs) = {
		val newCtx = ctx.inner()
		val paramTypes = params match {
			case Varargs(name) =>
				newCtx.add(name, TArray(TUnknown))
				None
			case Args(Nil) => Some(Nil)
			case Args(params2) =>
				val pairs = params2.map {
					case (None, name) => (TUnknown, name)
					case (Some(t), name) => (Type.fromExprType(t), name)
				}
				
				for((t, name) <- pairs) {
					newCtx.add(name, t)
				}
				
				Some(pairs.unzip._1)
		}
		
		(newCtx, paramTypes)
	}
	
	var quiet = false
	def warn(msg: String): Unit = if(!quiet) println(msg)
	
	
	def indexType(ty: Type): Type = ty match {
		case t if t == TUnknown => TUnknown
		case TUnion(types) => types.map(indexType).reduceLeft(_ | _)
		case TArray(_) => TInt
		case TObject => TUnknown
		case _ => TInvalid
	}
	
	def elementType(ty: Type): Type = ty match {
		case t if t == TUnknown => TUnknown
		case TUnion(types) => types.map(elementType).reduceLeft(_ | _)
		case TArray(elem) => elem
		case TObject => TUnknown
		case _ => TInvalid
	}
	
	class Typer(
		typeExpr: (Context, Expr) => Type,
		typeStmt: (Context, Stmt) => Type,
		isYield: Boolean,
		default: Type
	) {
		def typeExprWith(ctx: Context, expr: Expr): Type = expr match {
			/*case Expr.Block(stmts) =>
				val ctx2 = if(stmts.exists(_.isInstanceOf[Stmt.VarDecl])) ctx.inner() else ctx
				stmts.map(typeStmtWith(ctx2, _)).reduceLeft(_ | _)*/
			
			case Expr.For(Some(decl @ Stmt.VarDecl(_)), _, _, stmt) =>
				val newCtx = ctx.inner()
				typeStmtWith(newCtx, decl)
				typeStmtWith(newCtx, stmt)
			
			case Expr.ForXInY(isVar, x, y, stmt) =>
				val newCtx = ctx.inner()
				val yType = typeExprWith(newCtx, y)
				val xType = elementType(yType)
				
				if(isVar) {
					newCtx.add(x, xType)
				} else {
					if(!(newCtx contains x) && !ctx.outer.isEmpty) warn(s"warning: variable `$x` not found")
					newCtx.set(x, xType)
				}
				
				typeStmtWith(newCtx, stmt)
			
			case Expr.ForXYInZ(isVar, x, y, z, stmt) =>
				val newCtx = ctx.inner()
				val zType = typeExprWith(newCtx, z)
				val xType = indexType(zType)
				val yType = elementType(zType)
				
				if(isVar) {
					newCtx.add(x, xType)
					newCtx.add(y, yType)
				} else {
					if(!(newCtx contains x) && !ctx.outer.isEmpty) warn(s"warning: variable `$x` not found")
					if(!(newCtx contains y) && !ctx.outer.isEmpty) warn(s"warning: variable `$y` not found")
					newCtx.set(x, xType)
					newCtx.set(y, yType)
				}
				
				typeStmtWith(newCtx, stmt)
			
			case _ => typeExpr(ctx, expr)
		}
		
		def typeStmtWith(ctx: Context, stmt: Stmt): Type = stmt match {
			case expr: Expr => typeExprWith(ctx, expr)
			case _ => typeStmt(ctx, stmt)
		}
	}
	
	def typeFlowSwitch(
		ctx: Context,
		varName: String,
		cases: List[Expr.SwitchCase],
		typer: Typer
	): Type = {
		val varType = ctx.find(varName).get
		val varTypes = MutableMap.from(varType match {
			case t if t == TUnknown => Map(
				"$tnull" -> TNull,
				"$tint" -> TInt,
				"$tfloat" -> TFloat,
				"$tbool" -> TBool,
				"$tstring" -> TString,
				"$tarray" -> TArray(),
				"$tobject" -> TObject,
				"$tfunction" -> TFunction(),
				"$tabstract" -> TAbstract(None)
			)
			case TUnion(ts) => ts.groupMapReduce(t => t.name)(t => t)(_ | _)
			case TInvalid => Map[String, Type]()
			case _ => Map(varType.name -> varType)
		})
		cases map {
			case Expr.SwitchCase.Case(Expr.Builtin(builtin), stmt) =>
				val t = varTypes.remove(builtin).getOrElse(TInvalid)
				val newCtx = ctx.inner(varName -> t)
				funcTyper.typeStmtWith(newCtx, stmt)
			case Expr.SwitchCase.Case(_, stmt) => typer.typeStmtWith(ctx, stmt)
			case Expr.SwitchCase.Default(stmt) => typer.typeStmtWith(ctx, stmt)
		} reduceLeft {_ | _}
	}
	
	val funcTyper = new Typer(exprFuncRetType, stmtFuncRetType, true, TUnion(Set()))
	
	def exprFuncRetType(ctx: Context, expr: Expr): Type = Type.reduce(expr match {
		case Expr.Block(stmts) =>
			val ctx2 = if(stmts.exists(_.isInstanceOf[Stmt.VarDecl])) ctx.inner() else ctx
			stmts.map(funcTyper.typeStmtWith(ctx2, _)).reduceLeft {_ | _}
		case Expr.If(_, stmt) => funcTyper.typeStmtWith(ctx, stmt)
		case Expr.IfElse(_, stmt1, stmt2) => funcTyper.typeStmtWith(ctx, stmt1) | funcTyper.typeStmtWith(ctx, stmt2)
		case Expr.While(_, stmt) => funcTyper.typeStmtWith(ctx, stmt)
		case Expr.DoWhile(stmt, _) => funcTyper.typeStmtWith(ctx, stmt)
		case Expr.TryCatch(tryStmt, name, catchStmt) => funcTyper.typeStmtWith(ctx, tryStmt) | funcTyper.typeStmtWith(ctx.inner(name -> TUnknown), catchStmt)
		case Expr.Switch(_, Nil) => TUnion(Set())
		case Expr.Switch(Expr.Call(Expr.Builtin("$typeof"), List(Expr.Variable(varName))), cases) => typeFlowSwitch(ctx, varName, cases, funcTyper)
		case Expr.Switch(_, cases) => cases.map {
			case Expr.SwitchCase.Case(_, stmt) => funcTyper.typeStmtWith(ctx, stmt)
			case Expr.SwitchCase.Default(stmt) => funcTyper.typeStmtWith(ctx, stmt)
		} reduceLeft {_ | _}
		case _ => TUnion(Set())
	})
	
	def stmtFuncRetType(ctx: Context, stmt: Stmt): Type = Type.reduce(stmt match {
		case expr: Expr => funcTyper.typeExprWith(ctx, expr)
		case Stmt.Return(None) => TNull
		case Stmt.Return(Some(value)) => exprTyper.typeExprWith(ctx, value)
		case _ => TUnion(Set())
	})
	
	def funcRetType(ctx: Context, block: Expr.Block) = {
		funcTyper.typeExprWith(ctx, block) match {
			case TUnion(types) if types.isEmpty => funcTyper.typeExprWith(ctx, block)
			case t => Type.reduce(t)
		}
	}
	
	val exprTyper = new Typer(tryGetType, tryGetStmtType, false, TNull)
	
	def tryGetType(ctx: Context, expr: Expr): Type = Type.reduce(expr match {
		case Expr.Variable(name) if(ctx contains name) => ctx.find(name).getOrElse({
			if(ctx.outer.isDefined) warn(s"warning: variable `$name` not found")
			TUnknown
		})
		case Expr.Variable(name) =>
			warn(s"warning: variable `$name` not found")
			TNull
		
		case Expr.Builtin(builtin) => builtin.tail match {
			case "tnull" | "tint" | "tfloat" | "tbool" | "tstring" | "tobject" | "tarray" | "tfunction" | "tabstract" => TInt
			case "loader" | "exports" => TObject
			
			//array
			case "amake" => TFunction(Some(List(TInt)), TArray())
			//acopy
			case "asize" => TFunction(Some(List(TArray())), TInt)
			//asub,ablit,aconcat
			
			case "string" => TFunction(Some(List(TUnknown)), TString)
			case "smake" => TFunction(Some(List(TInt)), TString)
			case "ssize" => TFunction(Some(List(TString)), TInt)
			case "scopy" => TFunction(Some(List(TString)), TString)
			case "ssub" => TFunction(Some(List(TString, TInt, TInt)), TString)
			case "sget" => TFunction(Some(List(TString, TInt)), TNull | TInt)
			case "sget16" => TFunction(Some(List(TString, TInt, TBool)), TNull | TInt)
			case "sget32" => TFunction(Some(List(TString, TInt, TBool)), TNull | Type.anyInt)
			case "sgetf" | "sgetd" => TFunction(Some(List(TString, TInt, TBool)), TNull | TFloat)
			case "sset" => TFunction(Some(List(TString, TInt, TInt)), TNull | TInt)
			case "sset16" | "sset32" => TFunction(Some(List(TString, TInt, Type.anyInt, TBool)), TNull)
			case "ssetf" | "ssetd" => TFunction(Some(List(TString, TInt, TFloat, TBool)), TNull)
			case "sblit" => TFunction(Some(List(TString, TInt, TString, TInt, TInt)), TNull)
			case "sfind" => TFunction(Some(List(TString, TInt, TString)), TNull | TInt)
			
			case "new" => TFunction(Some(List(TObject | TNull)), TObject) // FIX
			case "objget" => TFunction(Some(List(TUnknown, TInt)), TUnknown)
			case "objset" => TFunction(Some(List(TUnknown, TInt, TUnknown)), TUnknown)
			case "objcall" => TFunction(Some(List(TUnknown, TInt, TArray())), TUnknown)
			case "objfield" => TFunction(Some(List(TUnknown, TInt)), TBool)
			case "objremove" => TFunction(Some(List(TObject, TInt)), TBool)
			case "objfields" => TFunction(Some(List(TObject)), TArray(TInt))
			case "hash" | "fasthash" => TFunction(Some(List(TString)), TInt)
			case "field" => TFunction(Some(List(TInt)), TString)
			case "objsetproto" => TFunction(Some(List(TObject, TObject | TNull)), TNull) // FIX
			case "objgetproto" => TFunction(Some(List(TObject)), TObject | TNull)
			
			case "nargs" => TFunction(Some(List(TFunction())), TInt)
			//call,closure,apply,varargs
			
			case "iadd" | "isub" | "imult" | "idiv" => TFunction(Some(List(TUnknown, TUnknown)), TInt)
			case "isnan" | "isinfinite" => TFunction(Some(List(TUnknown)), TBool)
			case "int" => union(
				TFunction(Some(List(TInt | TFloat)), TInt),
				TFunction(Some(List(TUnknown)), TInt | TNull)
			)
			case "float" => union(
				TFunction(Some(List(TInt | TFloat)), TFloat),
				TFunction(Some(List(TUnknown)), TFloat | TNull)
			)
			case "itof" => TFunction(Some(List(TInt | Type.tInt32, TBool)), TFloat)
			case "ftoi" => TFunction(Some(List(TFloat, TBool)), Type.anyInt)
			case "itod" => TFunction(Some(List(TInt | Type.tInt32, TInt | Type.tInt32, TBool)), TFloat)
			case "dtoi" => TFunction(Some(List(TFloat, TArray(TInt | Type.tKind))), TNull)
			case "isbigendian" => TFunction(Some(Nil), TBool)
			
			case "getkind" => TFunction(Some(List(TAbstract(None))), Type.tKind)
			case "iskind" => TFunction(Some(List(TUnknown, Type.tKind)), TBool)
			
			case "hkey" => TFunction(Some(List(TUnknown)), TInt)
			case "hnew" => TFunction(Some(List(TInt)), Type.tHash)
			case "hresize" => TFunction(Some(List(Type.tHash, TInt)), TNull)
			case "hget" | "hmem" | "hremove" => TFunction(Some(List(Type.tHash, TUnknown, TFunction(Some(List(TUnknown, TUnknown)), TUnknown) | TNull)), TUnknown)
			case "hset" => TFunction(Some(List(Type.tHash, TUnknown, TUnknown, TFunction(Some(List(TUnknown, TUnknown)), TUnknown) | TNull)), TBool)
			case "hadd" => TFunction(Some(List(Type.tHash, TUnknown, TUnknown)), TNull)
			case "hiter" => TFunction(Some(List(Type.tHash, TFunction(Some(List(TUnknown, TUnknown)), TUnknown))), TNull)
			case "hcount" | "hsize" => TFunction(Some(List(Type.tHash)), TInt)
			
			case "print" => TFunction(None, TNull)
			case "throw" | "rethrow" => TFunction(Some(List(TUnknown)), TUnion(Set())) // hacky thing because noreturn isn't a type yet
			case "istrue" | "not" => TFunction(Some(List(TUnknown)), TBool)
			case "typeof" => TFunction(Some(List(TUnknown)), TInt)
			case "compare" => TFunction(Some(List(TUnknown, TUnknown)), TNull | TInt)
			case "pcompare" => TFunction(Some(List(TUnknown, TUnknown)), TInt)
			case "excstack" | "callstack" => TFunction(Some(Nil), TArray())
			case "version" => TFunction(Some(Nil), TInt)
			case "setresolver" => TFunction(Some(List(TFunction(Some(List(TObject, TInt)), TUnknown) | TNull)), TNull)
			
			case _ =>
				warn(s"warning: unknown builtin `$builtin`")
				TFunction() // TODO: finish
		}
		
		case Expr.NekoValue.Null => TNull
		case Expr.NekoValue.Int(_) => TInt
		case Expr.NekoValue.Float(_) => TFloat
		case Expr.NekoValue.Bool(_) => TBool
		case Expr.NekoValue.String(_) => TString
		case Expr.NekoValue.Object(_) => TObject
		case Expr.NekoValue.This => TUnion(Set(TNull, TObject)) // add better logic for this later
		
		case _: Expr.ObjNValue => TObject
		
		case Expr.Func(_, Some(ret), _) => Type.fromExprType(ret) // TODO: check against inferred return type
		case Expr.Func(params, None, body) =>
			val (newCtx, paramTypes) = typeFuncParams(ctx, params)
			TFunction(paramTypes, funcRetType(newCtx, body))
		
		case _: Expr.Selector => TObject
		
		case Expr.Block(Nil) => TNull
		case Expr.Block(List(stmt)) => exprTyper.typeStmtWith(ctx, stmt)
		case Expr.Block(stmts) =>
			val ctx2 = if(stmts.exists(_.isInstanceOf[Stmt.VarDecl])) ctx.inner() else ctx
			stmts.init.foreach(exprTyper.typeStmtWith(ctx2, _))
			exprTyper.typeStmtWith(ctx2, stmts.last)
		
		case Expr.Box(e) => getBoxedType(ctx, exprTyper.typeExprWith(ctx, e))
		
		case Expr.Paren(e) => exprTyper.typeExprWith(ctx, e)
		
		case Expr.Call(Expr.Builtin(builtin), args)
		if Set("$array","$acopy","$asub","$ablit","$aconcat","$call","$closure","$apply","$varargs")(builtin) =>
		(builtin.tail, args.map(exprTyper.typeExprWith(ctx, _))) match {
			case ("array", values) => TArray(reduceUnion(values.toSet))
			case ("acopy", List(TArray(elem))) => TArray(elem)
			case ("asub", List(TArray(elem), TInt, TInt)) => TArray(elem)
			case ("ablit", List(TArray(elem1), TInt, TArray(elem2), TInt, TInt)) =>
				TArray(if(elem1 == elem2) elem1 else union(elem1, elem2))
			case ("aconcat", List(TArray(TArray(elem)))) => TArray(elem)
			case ("aconcat", List(TUnion(types))) if types.forall {
				case TArray(_) => true
				case _ => false
			} => TArray(TUnion(types.collect {
				case TArray(elem) => elem
			}))
			
			case ("call", List(TFunction(None, ret), _, TArray(_))) => ret
			case ("closure", TFunction(params, ret) :: _ :: Nil) => TFunction(params, ret)
			case ("closure", TFunction(Some(params), ret) :: _ :: args) =>
				if(args.length > params.length) {
					TInvalid
				} else if(params.corresponds(args) {_ accepts _}) {
					TFunction(Some(Nil), ret)
				} else if(params.take(args.length).corresponds(args) {_ accepts _}) {
					TFunction(Some(params.drop(args.length)), ret)
				} else {
					TInvalid
				}
			case ("apply", TFunction(None, ret) :: _) => ret
			case ("apply", TFunction(Some(Nil), ret) :: Nil) => ret
			case ("apply", TFunction(Some(params), ret) :: args) =>
				if(args.length > params.length) {
					TInvalid
				} else if(params.corresponds(args) {_ accepts _}) {
					ret
				} else if(params.take(args.length).corresponds(args) {_ accepts _}) {
					TFunction(Some(params.drop(args.length)), ret)
				} else {
					TInvalid
				}
			case ("varargs", List(TFunction(_, ret))) => TFunction(None, ret)
			
			case (_, argTypes) =>
				if(argTypes.exists(_ != TUnknown)) {
					warn(s"warning: possibly invalid call to builtin `$builtin` in `${expr.toObjn()}`")
					warn("\targument types: " + argTypes.map(_.toObjn()).mkString("(", ", ", ")"))
				}
				TUnknown
		}
		case Expr.Call(caller, args) =>
			val callerType = exprTyper.typeExprWith(ctx, caller)
			
			if(callerType == TUnknown) {
				TUnknown
			} else {
				callerType match {
					case TNull | TInt | TFloat | TBool | TString | TObject | TArray(_) | TAbstract(_) =>
						warn(s"warning: caller may not be a function (is: `${callerType.toObjn()}`) in `${expr.toObjn()}`")
					case _ =>
				}
				
				val funcTypes = callerType match {
					case TUnion(types) => types.collect { case TFunction(p, r) => (p, r) }.toList
					case TFunction(params, ret) => (params, ret) :: Nil
					case _ => Nil
				}
				
				funcTypes match {
					case Nil => TInvalid
					case (params, ret) :: Nil =>
						val argTypes = args.map(a => exprTyper.typeExprWith(ctx, a))
						if(params.isEmpty || params.get.corresponds(argTypes) {(p, a) => p accepts a}) {
							ret
						} else {
							warn(s"warning: argument list for function call may not be correct in `${expr.toObjn()}`")
							warn("\texpected: " + params.get.map(_.toObjn()).mkString("(", ", ", ")"))
							warn("\tfound:    " + argTypes.map(_.toObjn()).mkString("(", ", ", ")"))
							TInvalid
						}
					case _ =>
						warn("warning: function called has multiple potential types")
						val argTypes = args.map(a => exprTyper.typeExprWith(ctx, a))
						funcTypes.find {
							case (None, _) => true
							case (Some(params), _) => params.corresponds(argTypes) {(p, a) => p accepts a}
						} match {
							case Some((_, ret)) => ret
							case None =>
								warn(s"warning: argument list for function call may not be correct in `${expr.toObjn()}`")
								warn("\texpected one of:")
								for((Some(params), _) <- funcTypes)
									warn("\t\t" + params.map(_.toObjn()).mkString("(", ", ", ")"))
								warn("\tfound:")
								warn("\t\t" + argTypes.map(_.toObjn()).mkString("(", ", ", ")"))
								TInvalid
						}
				}
			}
		
		case Expr.GetIndex(value, index) => (exprTyper.typeExprWith(ctx, value), exprTyper.typeExprWith(ctx, index)) match {
			case (TUnion(types), indexType) =>
				if(types.forall {case TArray(_) => true; case _ => false}) {
					indexType match {
						case TUnion(ts) if ts(TInt) => TUnknown
						case TUnion(_) => TInvalid 
						case TInt => types collect {case TArray(elem) => elem} reduceLeft {_ | _}
						case TUnknown => TUnknown
						case _ => TInvalid
					}
				} else if(types.forall {case TArray(_) | TObject => true; case _ => false}) {
					TUnknown
				} else {
					TInvalid
				}
			case (TArray(elem), TInt) => elem
			case (TArray(_), TUnknown) => TUnknown
			case (TArray(_), _) => TInvalid
			case (TObject | TUnknown, _) => TUnknown
			case _ => TInvalid
		}
		
		case Expr.GetField(value, _) => exprTyper.typeExprWith(ctx, value) match {
			case TObject | TUnknown => TUnknown
			case _ => TInvalid
		}
		
		case Expr.Op.Infix(left, op, right) => (exprTyper.typeExprWith(ctx, left), op, exprTyper.typeExprWith(ctx, right)) match {
			case (TInt | TFloat, "/" | "**" | "/=" | "**=", TInt | TFloat) => TFloat
			case (TInt, "+" | "-" | "*" | "%" | "+=" | "-=" | "++=" | "--=" | "*=" | "%=" , TInt) => TInt
			case (TInt, "+" | "-" | "*" | "%" | "+=" | "-=" | "++=" | "--=" | "*=" | "%=" , TFloat) |
				(TFloat, "+" | "-" | "*" | "%" | "+=" | "-=" | "++=" | "--=" | "*=" | "%=" , TInt | TFloat) => TFloat
			case (TInt, "&" | "|" | "^" | ">>" | "<<" | "&=" | "|=" | "^=" | ">>=" | "<<=", TInt) => TInt
			case (TString, "+" | "+=" | "++=", _) => TString
			case (l, "&&" | "||" | "&&=" | "||=", r) => TUnion(Set(l, r))
			case (_, "==" | "!=" | ">" | ">=" | "<" | "<=", _) => TBool
			case (_, "=", r) => r
			case (TObject | TUnknown, _, _) | (_, _, TObject | TUnknown) => TUnknown
			case _ => TInvalid
		}
		
		case Expr.Op.Prefix(op, right) => (op, exprTyper.typeExprWith(ctx, right)) match {
			case (_, TObject | TUnknown) => TUnknown
			case ("!", _) => TBool
			case ("+" | "-", r @ (TInt | TFloat)) => r
			case ("~", TInt) => TInt
			case _ => TInvalid
		}
		
		case Expr.If(_, trueBlk) => exprTyper.typeStmtWith(ctx, trueBlk) | TNull
		
		case Expr.IfElse(_, trueBlk, falseBlk) => exprTyper.typeStmtWith(ctx, trueBlk) | exprTyper.typeStmtWith(ctx, falseBlk)
		
		case Expr.TryCatch(tryStmt, name, catchStmt) => exprTyper.typeStmtWith(ctx, tryStmt) | exprTyper.typeStmtWith(ctx.inner(name -> TUnknown), catchStmt)
		
		case Expr.Switch(_, Nil) => TNull
		case Expr.Switch(Expr.Call(Expr.Builtin("$typeof"), List(Expr.Variable(varName))), cases) => typeFlowSwitch(ctx, varName, cases, exprTyper)
		case Expr.Switch(_, cases) => cases map {
			case Expr.SwitchCase.Case(_, stmt) => exprTyper.typeStmtWith(ctx, stmt)
			case Expr.SwitchCase.Default(stmt) => exprTyper.typeStmtWith(ctx, stmt)
		} reduceLeft {_ | _}
		
		case Expr.Truthy(_) => TBool
		
		case Expr.Raw(_) => TUnknown
		
		case _ => TUnknown
	})
	
	def tryGetStmtType(ctx: Context, stmt: Stmt): Type = stmt match {
		case expr: Expr => exprTyper.typeExprWith(ctx, expr)
		
		case Stmt.VarDecl(decls) =>
			decls foreach {
				case (name, None, None) => ctx.add(name, TNull)
				case (name, Some(t), None) => ctx.add(name, Type.fromExprType(t))
				case (name, None, Some(value)) => ctx.add(name, exprTyper.typeExprWith(ctx, value))
				// TODO: actually check against expr type
				case (name, Some(t), Some(value)) => ctx.add(name, Type.fromExprType(t))
			}
			TNull
		
		case Stmt.FuncDecl(name, params, ret, body) =>
			// maybe this will work? idk need to figure out recursive functions
			val (newCtx, paramTypes) = typeFuncParams(ctx, params)
			
			if(!newCtx.contains(name)) {
				warn("thing "+name)
				ctx.add(name, TFunction(paramTypes, TUnknown))
			}
			
			val retType = ret.map(Type.fromExprType(_))
			ctx.add(name, TFunction(paramTypes, {
				val inferred = Checker.funcRetType(newCtx, body)
				retType match {
					case Some(ret) =>
						if(!(ret accepts inferred)) {
							warn("warning: type `"+inferred.toObjn()+"` doesn't match specified return type `"+ret.toObjn()+"`")
						}
						ret
					case None => inferred
				}
			}))
			TNull
		
		case Stmt.Interface(name, _, _) =>
			// TODO: fix once class types are added to inference
			ctx.add(name, TObject)
			TNull
		
		case Stmt.Implementation(_, _) => TNull
		
		case Stmt.Return(None) | Stmt.Break(None) | Stmt.Continue => TNull
		case Stmt.Return(Some(ret)) => exprTyper.typeExprWith(ctx, ret)
		case Stmt.Break(Some(ret)) => exprTyper.typeExprWith(ctx, ret)
		
		// TODO: respect imports in typer
		case Stmt.ImportLib(_, _) | Stmt.ImportFile(_, _) => TNull
		
		case _ => TInvalid
	}
	
	private def reduceUnion(types: Set[Type]): Type = {
		if(types.size == 1) 
			Type.reduce(types.head)
		else if(types(TInvalid) || types.size == 0)
			TInvalid
		else if(types(TUnknown))
			TUnknown
		else
			TUnion(types.map(Type.reduce))
	}
	
	private def union(types: Type*) = reduceUnion(types.toSet)
	
	private def getBoxedType(ctx: Context, typ: Type): Type = typ match {
		case TBool =>
			warn("warning: cannot box native type $tbool")
			TBool
		case TFunction(_, _) | TAbstract(_) => TInvalid
		case TInvalid | TUnknown => typ
		case TUnion(types) => reduceUnion(types.map(getBoxedType(ctx, _)))
		case _ => TObject
	}
}