package objn

import objn.Ast.{Context, Expr}

object Checker {
	sealed trait Type {
		def name: String
		def |(other: Type) = reduceUnion(Set(this, other))
		def accepts(other: Type) = other == this
		def isSubtypeOf(other: Type) = other accepts this
	}
	object Type {
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
	}
	case class TFunction(params: Option[List[Type]] = None, ret: Type = TUnknown) extends Type {
		def name = "$tfunction"
		override def accepts(other: Type) = other match {
			case TFunction(params, ret) => this.ret.accepts(ret) && ((this.params, params) match {
				case (None, _) => true
				case (Some(_), None) => false
				case (Some(p1), Some(p2)) if p1.length != p2.length => false
				case (Some(p1), Some(p2)) => p1.corresponds(p2) {(t1, t2) => t1 accepts t2}
			})
			case _ => false
		}
	}
	case class TAbstract(repr: Option[String]) extends Type { def name = "$tabstract" }
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
		def unapply(t: Type) = this accepts t
	}
	
	def tryGetType(ctx: Context, expr: Expr): Type = Type.reduce(expr match {
		case Expr.Builtin(builtin) => builtin match {
			case "$tnull" | "$tint" | "$tfloat" | "$tbool" | "$tstring" | "$tobject" | "$tarray" | "$tfunction" | "$tabstract" => TInt
			case "$loader" | "$exports" => TObject
			case _ => TFunction()
		}
		
		case Expr.NekoValue.Null => TNull
		case Expr.NekoValue.Int(_) => TInt
		case Expr.NekoValue.Float(_) => TFloat
		case Expr.NekoValue.Bool(_) => TBool
		case Expr.NekoValue.String(_) => TString
		case Expr.NekoValue.Object(_) => TObject
		case Expr.NekoValue.This => TUnion(Set(TNull, TObject)) // add better logic for this later
		
		case _: Expr.ObjNValue => TObject
		
		case Expr.Func(_, _) => TFunction()
		
		case _: Expr.Selector => TObject
		
		case Expr.Block(_) => TUnknown // TODO
		
		case Expr.Box(e) => getBoxedType(ctx, tryGetType(ctx, e))
		
		case Expr.Paren(e) => tryGetType(ctx, e)
		
		case Expr.Call(Expr.Builtin(builtin), args) => (builtin.tail, args.map(tryGetType(ctx, _))) match {
			case ("array", values) => TArray(reduceUnion(values.toSet))
			case ("amake", List(TInt)) => TArray()
			case ("acopy", List(TArray(elem))) => TArray(elem)
			case ("asize", List(TArray(_))) => TInt
			case ("asub", List(TArray(elem), TInt, TInt)) => TArray(elem)
			case ("ablit", List(TArray(elem1), TInt, TArray(elem2), TInt, TInt)) =>
				TArray(if(elem1 == elem2) elem1 else union(elem1, elem2))
			case ("aconcat", List(TArray(TArray(elem)))) => TArray(elem)
			
			case ("string", List(_)) => TString
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
			
			case ("nargs", List(TFunction(_, _))) => TInt
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
			
			case ("iadd" | "isub" | "imult" | "idiv", List(_, _)) => TInt
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
			case ("setresolver", List(TFunction(Some(List(TObject, TInt)), _) | TNull)) => TNull
			case _ => TUnknown
		}
		
		case Expr.GetField(value, _) => tryGetType(ctx, value) match {
			case TObject | TUnknown | TUnion(_) => TUnknown
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
		
		case _ => TUnknown
	})
	
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
		case TBool => TBool
		case TFunction(_, _) | TAbstract(_) => TInvalid
		case TInvalid | TUnknown => typ
		case TUnion(types) => reduceUnion(types.map(getBoxedType(ctx, _)))
		case _ => TObject
	}
}