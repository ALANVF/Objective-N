package objn

import objn.Ast.{Context, Expr, Statement}
import objn.Ast.Expr.FuncArgs.Args
import objn.Ast.Expr.FuncArgs.Varargs

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
			case Expr.Type.Union(types) => union(types.map(fromExprType) : _*)
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
				val (types, names) = params2.unzip {
					case (None, name) => (TUnknown, name)
					case (Some(t), name) => (Type.fromExprType(t), name)
				}
				
				var types2 = types
				for(name <- names) {
					val t :: tl = types
					newCtx.add(name, t)
					types2 = tl
				}
				
				Some(types)
		}
		
		(newCtx, paramTypes)
	}
	
	var quiet = false
	private def warn(msg: String): Unit = if(!quiet) println(msg)
	
	def exprFuncRetType(ctx: Context, expr: Expr): Type = Type.reduce(expr match {
		case Expr.Block(stmts) => stmts.map(stmt => stmtFuncRetType(ctx, stmt)).reduce(_ | _)
		case Expr.If(_, stmt) => stmtFuncRetType(ctx, stmt)
		case Expr.IfElse(_, stmt1, stmt2) => stmtFuncRetType(ctx, stmt1) | stmtFuncRetType(ctx, stmt2)
		case Expr.While(_, stmt) => stmtFuncRetType(ctx, stmt)
		case Expr.DoWhile(stmt, _) => stmtFuncRetType(ctx, stmt)
		case Expr.For(_, _, _, stmt) => stmtFuncRetType(ctx, stmt)
		case Expr.ForXInY(_, _, _, stmt) => stmtFuncRetType(ctx, stmt)
		case Expr.ForXYInZ(_, _, _, _, stmt) => stmtFuncRetType(ctx, stmt)
		case Expr.TryCatch(tryStmt, _, catchStmt) => stmtFuncRetType(ctx, tryStmt) | stmtFuncRetType(ctx, catchStmt)
		case Expr.Switch(_, Nil) => TUnion(Set())
		case Expr.Switch(_, cases) => cases.map {
			case Expr.SwitchCase.Case(_, stmt) => stmtFuncRetType(ctx, stmt)
			case Expr.SwitchCase.Default(stmt) => stmtFuncRetType(ctx, stmt)
		}.reduce(_ | _)
		case _ => TUnion(Set())
	})
	
	def stmtFuncRetType(ctx: Context, stmt: Statement): Type = Type.reduce(stmt match {
		case expr: Expr => exprFuncRetType(ctx, expr)
		case Statement.Return(None) => TNull
		case Statement.Return(Some(value)) => tryGetType(ctx, value)
		case _ => TUnion(Set())
	})
	
	def funcRetType(ctx: Context, block: Expr.Block) = {
		exprFuncRetType(ctx, block) match {
			case TUnion(types) if types.isEmpty => tryGetType(ctx, block)
			case t => Type.reduce(t)
		}
	}
	
	def tryGetType(ctx: Context, expr: Expr): Type = Type.reduce(expr match {
		case Expr.Variable(name) if(ctx contains name) => ctx.find(name).getOrElse(TUnknown)
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
			case "throw" | "rethrow" => TFunction(Some(List(TUnknown)), TUnknown)
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
		
		case Expr.Func(params, body) =>
			val (newCtx, paramTypes) = typeFuncParams(ctx, params)
			TFunction(paramTypes, funcRetType(newCtx, body))
		
		case _: Expr.Selector => TObject
		
		case Expr.Block(Nil) => TNull
		case Expr.Block(List(stmt)) => tryGetStmtType(ctx, stmt)
		case Expr.Block(stmts) => tryGetStmtType(ctx, stmts.last)
		
		case Expr.Box(e) => getBoxedType(ctx, tryGetType(ctx, e))
		
		case Expr.Paren(e) => tryGetType(ctx, e)
		
		case Expr.Call(Expr.Builtin(builtin), args)
		if Set("$array","$acopy","$asub","$ablit","$aconcat","$call","$closure","$apply","$varargs")(builtin) =>
		(builtin.tail, args.map(tryGetType(ctx, _))) match {
			case ("array", values) => TArray(reduceUnion(values.toSet))
			//case ("amake", List(TInt)) => TArray()
			case ("acopy", List(TArray(elem))) => TArray(elem)
			//case ("asize", List(TArray(_))) => TInt
			case ("asub", List(TArray(elem), TInt, TInt)) => TArray(elem)
			case ("ablit", List(TArray(elem1), TInt, TArray(elem2), TInt, TInt)) =>
				TArray(if(elem1 == elem2) elem1 else union(elem1, elem2))
			case ("aconcat", List(TArray(TArray(elem)))) => TArray(elem)
			
			/*case ("string", List(_)) => TString
			case ("smake", List(TInt)) => TString
			case ("ssize", List(TString)) => TInt
			case ("scopy", List(TString)) => TString
			case ("ssub", List(TString, TInt, TInt)) => TString
			case ("sget", List(TString, TInt)) => union(TNull, TInt)
			case ("sget16", List(TString, TInt, TBool)) => union(TNull, TInt)
			case ("sget32", List(TString, TInt, TBool)) => union(TNull, Type.anyInt)
			case ("sgetf" | "sgetd", List(TString, TInt, TBool)) => union(TNull, TFloat)
			case ("sset", List(TString, TInt, TInt)) => union(TNull, TInt)
			case ("sset16" | "sset32", List(TString, TInt, Type.anyInt, TBool)) => TNull
			case ("ssetf" | "ssetd", List(TString, TInt, TFloat, TBool)) => TNull
			case ("sblit", List(TString, TInt, TString, TInt, TInt)) => TNull
			case ("sfind", List(TString, TInt, TString)) => union(TNull, TInt)
			
			case ("new", List(TObject | TNull)) => TObject // FIX
			case ("objget", List(_, TInt)) => TUnknown
			case ("objset", List(_, TInt, _)) => TUnknown
			case ("objcall", List(_, TInt, TArray(_))) => TUnknown
			case ("objfield", List(_, TInt)) => TBool
			case ("objremove", List(TObject, TInt)) => TBool
			case ("objfields", List(TObject)) => TArray(TInt)
			case ("hash" | "fasthash", List(TString)) => TInt
			case ("field", List(TInt)) => TString
			case ("objsetproto", List(TObject, TObject | TNull)) => TNull // FIX
			case ("objgetproto", List(TObject)) => union(TObject, TNull)
			
			case ("nargs", List(TFunction(_, _))) => TInt*/
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
			
			/*case ("iadd" | "isub" | "imult" | "idiv", List(_, _)) => TInt
			case ("isnan" | "isinfinite", List(_)) => TBool
			case ("int", List(TInt | TFloat)) => TInt
			case ("int", List(_)) => union(TNull, TInt)
			case ("float", List(TInt | TFloat)) => TFloat
			case ("float", List(_)) => union(TNull, TFloat)
			case ("itof", List(TInt | Type.tInt32, TBool)) => TFloat
			case ("ftoi", List(TFloat, TBool)) => Type.anyInt
			case ("itod", List(TInt | Type.tInt32, TInt | Type.tInt32, TBool)) => TFloat
			case ("dtoi", List(TFloat, TArray(TInt | Type.tKind))) => TNull
			case ("isbigendian", Nil) => TBool
			
			case ("getkind", List(TAbstract(_))) => Type.tKind
			case ("iskind", List(_, Type.tKind)) => TBool
			
			case ("hkey", List(_)) => TInt
			case ("hnew", List(TInt)) => Type.tHash
			case ("hresize", List(Type.tHash, TInt)) => TNull
			case ("hget" | "hmem" | "hremove", List(Type.tHash, _, TFunction(Some(List(_, _)), _) | TNull)) => TUnknown
			case ("hset", List(Type.tHash, _, _, TFunction(Some(List(_, _)), _) | TNull)) => TBool
			case ("hadd", List(Type.tHash, _, _)) => TNull
			case ("hiter", List(Type.tHash, TFunction(Some(List(_, _)), _))) => TNull
			case ("hcount" | "hsize", List(Type.tHash)) => TInt
			
			case ("print", _) => TNull
			case ("throw" | "rethrow", List(_)) => TUnknown
			case ("istrue" | "not", List(_)) => TBool
			case ("typeof", List(_)) => TInt
			case ("compare", List(_, _)) => TNull | TInt
			case ("pcompare", List(_, _)) => TInt
			case ("excstack" | "callstack", Nil) => TArray()
			case ("version", Nil) => TInt
			case ("setresolver", List(TFunction(Some(List(TObject, TInt)), _) | TNull)) => TNull*/
			case (_, argTypes) =>
				if(argTypes.exists(t => t != TUnknown)) {
					warn(s"warning: possibly invalid call to builtin `$builtin` in `${expr.toObjn()}`")
					warn("\targument types: " + argTypes.map(_.toObjn()).mkString("(", ", ", ")"))
				}
				TUnknown
		}
		case Expr.Call(caller, args) =>
			val callerType = tryGetType(ctx, caller)
			
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
						val argTypes = args.map(a => tryGetType(ctx, a))
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
						val argTypes = args.map(a => tryGetType(ctx, a))
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
		
		case Expr.GetIndex(value, index) => (tryGetType(ctx, value), tryGetType(ctx, index)) match {
			case (TArray(elem), TInt) => elem
			case (TArray(_), TUnknown) => TUnknown
			case (TArray(_), _) => TInvalid
			case (TObject | TUnknown, _) => TUnknown
			case _ => TInvalid
		}
		
		case Expr.GetField(value, _) => tryGetType(ctx, value) match {
			case TObject | TUnknown => TUnknown
			case _ => TInvalid
		}
		
		case Expr.Op.Infix(left, op, right) => (tryGetType(ctx, left), op, tryGetType(ctx, right)) match {
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
		
		case Expr.Op.Prefix(op, right) => (op, tryGetType(ctx, right)) match {
			case (_, TObject | TUnknown) => TUnknown
			case ("!", _) => TBool
			case ("+" | "-", r @ (TInt | TFloat)) => r
			case ("~", TInt) => TInt
			case _ => TInvalid
		}
		
		case Expr.If(_, trueBlk) => tryGetStmtType(ctx, trueBlk) | TNull
		
		case Expr.IfElse(_, trueBlk, falseBlk) => tryGetStmtType(ctx, trueBlk) | tryGetStmtType(ctx, falseBlk)
		
		case Expr.TryCatch(tryStmt, _, catchStmt) => tryGetStmtType(ctx, tryStmt) | tryGetStmtType(ctx, catchStmt)
		
		case Expr.Switch(_, Nil) => TNull
		case Expr.Switch(_, cases) => cases map {
			case Expr.SwitchCase.Case(_, stmt) => tryGetStmtType(ctx, stmt)
			case Expr.SwitchCase.Default(stmt) => tryGetStmtType(ctx, stmt)
		} reduceLeft {_ | _}
		
		case Expr.Truthy(_) => TBool
		
		case Expr.Raw(_) => TUnknown
		
		case _ => TUnknown
	})
	
	def tryGetStmtType(ctx: Context, stmt: Statement): Type = stmt match {
		case expr: Expr => tryGetType(ctx, expr)
		
		case Statement.VarDecl(decls) =>
			decls foreach {
				case (name, None) => ctx.add(name, TNull)
				case (name, Some(value)) => ctx.add(name, tryGetType(ctx, value))
			}
			TNull
		
		case Statement.FuncDecl(name, params, body) =>
			// maybe this will work? idk need to figure out recursive functions
			val (newCtx, paramTypes) = typeFuncParams(ctx, params)
			if(!ctx.contains(name)) ctx.add(name, TFunction(paramTypes, TUnknown))
			ctx.add(name, TFunction(paramTypes, funcRetType(newCtx, body)))
			TNull
		
		case Statement.Interface(name, _, _) =>
			// TODO: fix once class types are added to inference
			ctx.add(name, TObject)
			TNull
		
		case Statement.Implementation(_, _) => TNull
		
		case Statement.Return(None) | Statement.Break(None) | Statement.Continue => TNull
		case Statement.Return(Some(ret)) => tryGetType(ctx, ret)
		case Statement.Break(Some(ret)) => tryGetType(ctx, ret)
		
		// TODO: respect imports in typer
		case Statement.ImportLib(_, _) | Statement.ImportFile(_, _) => TNull
		
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