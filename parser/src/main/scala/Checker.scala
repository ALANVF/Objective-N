package objn

import objn.Ast.{Context, Expr}

object Checker {
	sealed trait Type { def name: String }
	object Type {
		case object TNull extends Type { def name = "$tnull" }
		case object TInt extends Type { def name = "$tint" }
		case object TFloat extends Type { def name = "$tfloat" }
		case object TBool extends Type { def name = "$tbool" }
		case object TString extends Type { def name = "$tstring" }
		case object TObject extends Type { def name = "$tobject" }
		case object TArray extends Type { def name = "$tarray" }
		case object TFunction extends Type { def name = "$tfunction" }
		case object TAbstract extends Type { def name = "$tabstract" }
		// Maybe add objn object types later...
		case object Unknown extends Type { def name = "???" }
		case object Invalid extends Type { def name = "!!!" }
		case class Union(types: Set[Type]) extends Type {
			//def this(types: Type*) = this(types.toSet)
			def name = types.map(_.name).mkString("(", " | ", ")")
		}
		
		def reduce(typ: Type) = typ match {
			case Union(types) => reduceUnion(types)
			case _ => typ
		}
	}
	
	def tryGetType(ctx: Context, expr: Expr): Type = Type.reduce(expr match {
		case Expr.Builtin(builtin) => builtin match {
			case "$tnull" | "$tint" | "$tfloat" | "$tbool" | "$tstring" | "$tobject" | "$tarray" | "$tfunction" | "$tabstract" => Type.TInt
			case "$loader" | "$exports" => Type.TObject
			case _ => Type.TFunction
		}
		
		case Expr.NekoValue.Null => Type.TNull
		case Expr.NekoValue.Int(_) => Type.TInt
		case Expr.NekoValue.Float(_) => Type.TFloat
		case Expr.NekoValue.Bool(_) => Type.TBool
		case Expr.NekoValue.String(_) => Type.TString
		case Expr.NekoValue.Object(_) => Type.TObject
		case Expr.NekoValue.This => Type.Union(Set(Type.TNull, Type.TObject)) // add better logic for this later
		
		case _: Expr.ObjNValue => Type.TObject
		
		case Expr.Func(_, _) => Type.TFunction
		
		case _: Expr.Selector => Type.TObject
		
		case Expr.Block(_) => Type.Unknown // TODO
		
		case Expr.Box(e) => getBoxedType(ctx, tryGetType(ctx, e))
		
		case Expr.Paren(e) => tryGetType(ctx, e)
		
		case Expr.Call(Expr.Builtin(builtin), _) => builtin match { // TODO: be more complete about this
			case "$int" => Type.TInt
			case "$float" => Type.TFloat
			case "$string" => Type.TString
			case "$new" => Type.TObject // not completely accurate but eh
			case "$array" => Type.TArray
			case "$istrue" | "$not" => Type.TBool
			case _ => Type.Unknown
		}
		
		case Expr.GetField(value, _) => tryGetType(ctx, value) match {
			case Type.TObject | Type.Unknown | Type.Union(_) => Type.Unknown
			case _ => Type.Invalid
		}
		
		case Expr.Op.Infix(left, op, right) => (tryGetType(ctx, left), op, tryGetType(ctx, right)) match {
			case (Type.TInt | Type.TFloat, "/" | "**" | "/=" | "**=", Type.TInt | Type.TFloat) => Type.TFloat
			case (Type.TInt, "+" | "-" | "*" | "%" | "+=" | "-=" | "++=" | "--=" | "*=" | "%=" , Type.TInt) => Type.TInt
			case (Type.TInt, "+" | "-" | "*" | "%" | "+=" | "-=" | "++=" | "--=" | "*=" | "%=" , Type.TFloat) |
				(Type.TFloat, "+" | "-" | "*" | "%" | "+=" | "-=" | "++=" | "--=" | "*=" | "%=" , Type.TInt | Type.TFloat) => Type.TFloat
			case (Type.TInt, "&" | "|" | "^" | ">>" | "<<" | "&=" | "|=" | "^=" | ">>=" | "<<=", Type.TInt) => Type.TInt
			case (Type.TString, "+" | "+=" | "++=", _) => Type.TString
			case (l, "&&" | "||" | "&&=" | "||=", r) => Type.Union(Set(l, r))
			case (_, "==" | "!=" | ">" | ">=" | "<" | "<=", _) => Type.TBool
			case (_, "=", r) => r
			case (Type.TObject | Type.Unknown, _, _) | (_, _, Type.TObject | Type.Unknown) => Type.Unknown
			case _ => Type.Invalid
		}
		
		case Expr.Op.Prefix(op, right) => (op, tryGetType(ctx, right)) match {
			case (_, Type.TObject | Type.Unknown) => Type.Unknown
			case ("!", _) => Type.TBool
			case ("+" | "-", r @ (Type.TInt | Type.TFloat)) => r
			case ("~", Type.TInt) => Type.TInt
			case _ => Type.Invalid
		}
		
		case _ => Type.Unknown
	})
	
	private def reduceUnion(types: Set[Type]): Type = {
		if(types.size == 1) 
			Type.reduce(types.head)
		else if(types(Type.Invalid) || types.size == 0)
			Type.Invalid
		else if(types(Type.Unknown))
			Type.Unknown
		else
			Type.Union(types.map(Type.reduce))
	}
	
	private def getBoxedType(ctx: Context, typ: Type): Type = typ match {
		case Type.TBool => Type.TBool
		case Type.TFunction | Type.TAbstract => Type.Invalid
		case Type.Invalid | Type.Unknown => typ
		case Type.Union(types) => reduceUnion(types.map(getBoxedType(ctx, _)))
		case _ => Type.TObject
	}
}