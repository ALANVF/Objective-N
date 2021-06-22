package objn

import scala.collection.Map
import scala.collection.mutable.{Map => MutableMap}
import scala.io.BufferedSource

package object Ast {
	def singleSel(name: String) = s"@SEL(0, $$array(" + '"' + name + "\"))"
	def multiSel(names: List[String]): String = {
		val ks = array(names.map('"' + _ + '"'))
		
		return s"@SEL(${names.length}, $ks)"
	}
	
	def array(values: String*) = values.mkString("$array(", ",", ")")
	def array(values: List[String]) = values.mkString("$array(", ",", ")")
	
	def truthy(ctx: Context, expr: Expr): String = {
		import Checker.{TNull, TInt, TFloat, TString, TBool, TObject, TArray, TFunction, TAbstract, TUnknown}
		
		Checker.tryGetType(ctx, expr) match {
			case TBool => expr.toNeko(ctx)
			//case TObject | TUnknown => "@BOOL(" + expr.toNeko(ctx) + ")"
			case _ => "$istrue(" + expr.toNeko(ctx) + ")"
		}
	}
	
	class Context(outer: Option[Context]) {
		import Checker.{Type}
		
		var genSyms: Int = outer.map(_.genSyms).getOrElse(0)
		var imports = MutableMap[String, String]()
		var locals = MutableMap[String, Type]()
		
		def this() = this(None)
		def this(outer: Context) = this(Some(outer))
		
		def genSym[T](blk: (String) => T): T = {
			genSyms += 1
			val res = blk("@anon" + genSyms)
			genSyms -= 1
			return res
		}
		
		def get(name: String) = locals(name)
		
		def find(name: String): Option[Type] = locals.get(name) match {
			case found @ Some(_) => found
			case None => outer.flatMap(_.find(name))
		}
		
		def add(name: String, t: Type) = locals(name) = t
		
		def set(name: String, t: Type): Unit = outer match {
			case Some(_) if(locals contains name) => locals(name) = t
			case Some(outer) => outer.set(name, t)
			case None => locals(name) = t // Global variable
		}
		
		def contains(name: String): Boolean = locals.contains(name) || outer.forall(_ contains name)
		
		def inner() = new Context(this)
		def inner[T](blk: (Context) => T) = blk(new Context(this))
		def inner(pairs: (String, Type)*) = {
			val ctx = new Context(this)
			for((n, t) <- pairs) ctx.add(n, t)
			ctx
		}
	}
	
	private val TAB = "\t"
	sealed trait Expr extends Statement {
		import Expr._
		
		override def toObjn(tabs: String = ""): String = this match {
			case NekoValue.Object(pairs) =>
				pairs.map {case (k, v) => s"\n$tabs$TAB$k => ${v.toObjn(tabs+TAB)}"}.mkString("{", ",", "}")
			
			case ObjNValue.Nil => "nil"
			case ObjNValue.Null => "NULL"
			case ObjNValue.String(s) => "@\""+s+'"'
			case ObjNValue.Float(f) => s"@$f"
			case ObjNValue.Int(i) => s"@$i"
			case ObjNValue.Array(values) =>
				val buf = new StringBuilder("@[")
				val tabs2 = tabs + TAB
				for(v <- values) buf.append('\n' + tabs2 + v.toObjn(tabs2))
				buf.append('\n' + tabs + ']').toString()
			case ObjNValue.Dict(pairs) =>
				val buf = new StringBuilder("@{")
				val tabs2 = tabs + TAB
				for((k, v) <- pairs) buf.append('\n' + tabs2 + k.toObjn(tabs2) + ": " + v.toObjn(tabs2))
				buf.append('\n' + tabs + '}').toString()
			
			case Func(params, body) =>
				(params match {
					case FuncArgs.Args(Nil) => "^" 
					case FuncArgs.Args(params2) => params2.map {
						case (None, name) => name
						case (Some(t), name) => t.toObjn + ' ' + name
					}.mkString("^(", ", ", ") ")
					case FuncArgs.Varargs(name) => s"^($name...) "
				}) + body.toObjn(tabs)
			
			case Selector.Single(name) => s"@selector($name)"
			case Selector.Multi(names) => names.mkString("@selector(", ":", ":)")
			
			case Block(stmts) =>
				val buf = new StringBuilder("{")
				val tabs2 = tabs + TAB
				for(s <- stmts) buf.append('\n' + tabs2 + s.toObjn(tabs2))
				buf.append('\n' + tabs + '}').toString()
			
			case Box(v) => "@(" + v.toObjn(tabs) + ')'
			
			case Paren(v) => '(' + v.toObjn(tabs) + ')'
			
			case Call(caller, args) => caller.toObjn(tabs) + args.map(_.toObjn(tabs)).mkString("(", ", ", ")")
			
			case GetIndex(value, index) => value.toObjn(tabs) + '[' + index.toObjn(tabs) + ']'
			
			case GetField(value, field) => value.toObjn(tabs) + '.' + field
			
			case GetMember(field) => s"this->$field"
			
			case Op.Infix(l, op, r) => l.toObjn(tabs) + s" $op " + r.toObjn(tabs)
			case Op.Prefix(op, r) => op + r.toObjn(tabs)
			
			case Message.Single(sender, name) => '[' + sender.toObjn(tabs) + s" $name]"
			case Message.Multi(sender, args) => '[' + sender.toObjn(tabs) + (args match {
				case List((label, value)) => ' ' + label + ':' + value.toObjn(tabs)
				case _ =>
					val tabs2 = tabs+TAB
					args.map {case (label, value) => '\n' + tabs2 + label + value.toObjn(tabs2)}.mkString
			}) + ']'
			
			case If(cond, thenStmt) => "if " + cond.toObjn(tabs) + ' ' + thenStmt.toObjn(tabs)
			
			case IfElse(cond, thenStmt, elseStmt) => "if " + cond.toObjn(tabs) + ' ' + thenStmt.toObjn(tabs) + " else " + elseStmt.toObjn(tabs)
			
			case While(cond, stmt) => "while " + cond.toObjn(tabs) + ' ' + stmt.toObjn(tabs)
			
			case DoWhile(stmt, cond) => "do " + stmt.toObjn(tabs) + " while " + cond.toObjn(tabs)
			
			case For(decl, cond, update, stmt) =>
				"for(" +
				decl.map(_.toObjn(tabs)) +
				"; " +
				cond.map(_.toObjn(tabs)) +
				"; " +
				update.map(_.toObjn(tabs)) +
				") " +
				stmt.toObjn(tabs)
			
			case ForXInY(newX, x, y, stmt) => (if(newX) s"for(var $x in " else s"for($x in ") + y.toObjn(tabs) + ") " + stmt.toObjn(tabs)
			
			case ForXYInZ(newXY, x, y, z, stmt) => (if(newXY) s"for(var $x, $y in " else s"for($x, $y in ") + z.toObjn(tabs) + ") " + stmt.toObjn(tabs)
			
			case TryCatch(tryStmt, name, catchStmt) => "try " + tryStmt.toObjn(tabs) + s" catch $name " + catchStmt.toObjn(tabs)
			
			case Switch(target, cases) =>
				val buf = new StringBuilder("switch " + target.toObjn(tabs) + " {")
				val tabs2 = tabs + TAB
				for(c <- cases) {
					buf
					.append('\n' + tabs2)
					.append(c match {
						case SwitchCase.Case(value, stmt) => value.toObjn(tabs2) + " => " + stmt.toObjn(tabs2)
						case SwitchCase.Default(stmt) => "default => " + stmt.toObjn(tabs2)
					})
				}
				buf
				.append('\n' + tabs + '}')
				.toString()
			
			case Truthy(value) => "?("+value.toObjn(tabs)+')'
			
			case Raw(code) => s"@neko($code)"
			
			case _ => this.toNeko(null) // hack because I'm lazy
		}
	}
	object Expr {
		// Basic values:
		case class Name(name: String)     extends Expr { def toNeko(ctx: Context) = name }
		case class Variable(name: String) extends Expr { def toNeko(ctx: Context) = name }
		case class Builtin(name: String)  extends Expr { def toNeko(ctx: Context) = name }
		case class Keyword(name: String)  extends Expr { def toNeko(ctx: Context) = name }
		
		sealed trait Type {
			def toNeko: String
			def toObjn: String = this match {
				case Type.Builtin(name) => name
				case Type.Compound(path) => path.mkString(".")
				case Type.Union(types) => types.map(_.toObjn).mkString(" | ")
			}
		}
		object Type {
			case class Builtin(name: String)        extends Type { def toNeko = name }
			case class Compound(path: List[String]) extends Type { def toNeko = path.mkString(".") }
			case class Union(types: List[Type])     extends Type { def toNeko = types.map(_.toNeko).mkString("$array(", ",", ")") }
		}
		
		sealed trait NekoValue extends Expr
		object NekoValue {
			case class String(str: java.lang.String)            extends NekoValue { def toNeko(ctx: Context) = '"' + str + '"' }
			case class Float(float: Double)                     extends NekoValue { def toNeko(ctx: Context) = float.toString }
			case class Int(int: scala.Int)                      extends NekoValue { def toNeko(ctx: Context) = int.toString }
			case class Bool(bool: Boolean)                      extends NekoValue { def toNeko(ctx: Context) = bool.toString }
			case object Null                                    extends NekoValue { def toNeko(ctx: Context) = "null" }
			case object This                                    extends NekoValue { def toNeko(ctx: Context) = "this" }
			case class Object(pairs: List[(java.lang.String, Expr)]) extends NekoValue {
				def toNeko(ctx: Context) = pairs.map {
					case (k, v) => s"$k=>${v.toNeko(ctx)}"
				}.mkString("{", ",", "}")
			}
		}
		
		sealed trait ObjNValue extends Expr
		object ObjNValue {
			case object Nil                          extends ObjNValue { def toNeko(ctx: Context) = Message.send("ON_Nil", "make") }
			case object Null                         extends ObjNValue { def toNeko(ctx: Context) = Message.send("ON_Null", "make") }
			case class String(str: java.lang.String) extends ObjNValue { def toNeko(ctx: Context) = Message.send("ON_String", List(("stringWithNekoString", '"' + str + '"'))) }
			case class Float(float: Double)          extends ObjNValue { def toNeko(ctx: Context) = Message.send("ON_Float", List(("floatWithNekoFloat", float.toString))) }
			case class Int(int: scala.Int)           extends ObjNValue { def toNeko(ctx: Context) = Message.send("ON_Integer", List(("integerWithNekoInteger", int.toString))) }
			case class Array(values: List[Expr])     extends ObjNValue {
				def toNeko(ctx: Context) = values match {
					case List() => Message.send("ON_Array", "array")
					case List(value) => Message.send("ON_Array", List(("arrayWithValue", value.toNeko(ctx))))
					case _ => Message.send("ON_Array", List(("arrayWithValues", array(values.map(_.toNeko(ctx))))))
				}
			}
			case class Dict(pairs: List[(Expr, Expr)]) extends ObjNValue {
				def toNeko(ctx: Context) = pairs match {
					case List() => Message.send("ON_Dictionary", "dictionary")
					case List((key, value)) => Message.send("ON_Dictionary", List(("dictionaryWithValue", value.toNeko(ctx)), ("forKey", key.toNeko(ctx))))
					case _ => {
						val (keys, values) = pairs.unzip
						Message.send(
							"ON_Dictionary",
							List(
								("dictionaryWithValues", array(values.map(_.toNeko(ctx)))),
								("forKeys", array(keys.map(_.toNeko(ctx))))
							)
						)
					}
				}
			}
		}
		
		sealed trait FuncArgs
		object FuncArgs {
			case class Args(args: List[(Option[Type], String)]) extends FuncArgs
			case class Varargs(name: String)                    extends FuncArgs
		}
		
		case class Func(args: FuncArgs, body: Block) extends Expr {
			def toNeko(ctx: Context): String =
				this.args match {
					case FuncArgs.Args(List()) => s"(function()${this.body.toNeko(ctx)})"
					case FuncArgs.Args(args) if args.forall(_._1.isEmpty) => s"(function(${args.map(_._2).mkString(",")})${this.body.toNeko(ctx)})"
					case FuncArgs.Args(args) => {
						val checks = args.flatMap {
							case (Some(t), name) => Some(s"objn_Typecheck(${t.toNeko},$name,false);")
							case _ => None
						}.mkString
						
						return s"(function(${args.map(_._2).mkString(",")})${this.body.toNeko(ctx, checks)})"
					}
					case FuncArgs.Varargs(arg) => s"$$varargs(function($arg)${this.body.toNeko(ctx)})"
				}
		}
		
		sealed trait Selector extends Expr
		object Selector {
			case class Single(name: String) extends Selector {
				def toNeko(ctx: Context) = "@SEL(0, $array(\"" + name + "\"))"
			}
			case class Multi(names: List[String]) extends Selector {
				def toNeko(ctx: Context) = String.join("",
					s"@SEL(${names.length},$$array(",
						names.map('"' + _ + '"').mkString(","),
					"))"
				)
			}
		}
		
		case class Block(stmts: List[Statement]) extends Expr {
			private def mkStmts(ctx: Context) = stmts match {
				case List() => ""
				case List(stmt) => stmt.toNeko(ctx)
				case _ => stmts.map(_.toNeko(ctx)).mkString(";\n")
			}
			def toNeko(ctx: Context) = '{' + mkStmts(ctx.inner()) + '}'
			def toNeko(ctx: Context, prepend: String) = '{' + prepend + mkStmts(ctx.inner()) + '}'
		}
		
		
		// Basic expressions:
		case class Box(value: Expr) extends Expr { def toNeko(ctx: Context) = s"ON_BoxValue(${value.toNeko(ctx)})" }
		
		case class Paren(value: Expr) extends Expr { def toNeko(ctx: Context) = s"(${value.toNeko(ctx)})" }
		
		case class Call(caller: Expr, params: List[Expr]) extends Expr {
			def toNeko(ctx: Context) = params.map(_.toNeko(ctx)).mkString(caller.toNeko(ctx) + '(', ",", ")")
		}
		
		case class GetIndex(value: Expr, index: Expr) extends Expr {
			def toNeko(ctx: Context) = value.toNeko(ctx) + '[' + index.toNeko(ctx) + ']'
		}
		
		case class GetField(value: Expr, field: String) extends Expr {
			def toNeko(ctx: Context) = value.toNeko(ctx) + '.' + field
		}
		
		case class GetMember(field: String) extends Expr {
			def toNeko(ctx: Context) = s"this.@@get_vars().$field"
		}
		
		
		// Operators:
		sealed trait Op extends Expr
		object Op {
			case class Infix(left: Expr, symbol: String, right: Expr) extends Op {
				def toNeko(ctx: Context) = {
					import Checker.{TNull, TInt, TFloat, TString, TBool, TObject, TArray, TFunction, TAbstract, TUnknown, TInvalid, TUnion}
					symbol match {
						case "+" | "-" | "*" | "/" | "%" | "&&" | "||" |
							"==" | "!=" | ">=" | "<=" | ">" | "<" |
							"=" | "+=" | "-="  | "++=" | "--=" | "*=" | "/=" | "%=" | "&&=" | "||=" => left.toNeko(ctx) + symbol + right.toNeko(ctx)
						case "**" => s"@POW(${left.toNeko(ctx)},${right.toNeko(ctx)})"
						case "<<" | ">>" | "&" | "|" | "^" | "<<=" | ">>=" | "&=" | "|=" | "^="
							if Checker.tryGetType(ctx, left) == TInt && Checker.tryGetType(ctx, right) == TInt =>
								left.toNeko(ctx) + symbol + right.toNeko(ctx)
						case "<<" => s"@SHL(${left.toNeko(ctx)},${right.toNeko(ctx)})"
						case ">>" => s"@SHR(${left.toNeko(ctx)},${right.toNeko(ctx)})"
						case "&" => s"@BITAND(${left.toNeko(ctx)},${right.toNeko(ctx)})"
						case "|" => s"@BITOR(${left.toNeko(ctx)},${right.toNeko(ctx)})"
						case "^" => s"@BITXOR(${left.toNeko(ctx)},${right.toNeko(ctx)})"
						case "**=" => s"${left.toNeko(ctx)}=@POW(${left.toNeko(ctx)},${right.toNeko(ctx)})"   // TODO: fix
						case "<<=" => s"${left.toNeko(ctx)}=@SHL(${left.toNeko(ctx)},${right.toNeko(ctx)})"   // TODO: fix
						case ">>=" => s"${left.toNeko(ctx)}=@SHR(${left.toNeko(ctx)},${right.toNeko(ctx)})"   // TODO: fix
						case "&=" => s"${left.toNeko(ctx)}=@BITAND(${left.toNeko(ctx)},${right.toNeko(ctx)})" // TODO: fix
						case "|=" => s"${left.toNeko(ctx)}=@BITOR(${left.toNeko(ctx)},${right.toNeko(ctx)})"  // TODO: fix
						case "^=" => s"${left.toNeko(ctx)}=@BITXOR(${left.toNeko(ctx)},${right.toNeko(ctx)})" // TODO: fix
						case _ => scala.sys.error(s"Unknown operator `$symbol`!")
					}
				}
			}
			
			case class Prefix(symbol: String, right: Expr) extends Op {
				def toNeko(ctx: Context) = {
					import Checker.{TNull, TInt, TFloat, TString, TBool, TObject, TArray, TFunction, TAbstract, TUnknown, TInvalid, TUnion}
					symbol match {
						case "!" => Checker.tryGetType(ctx, right) match {
							case TObject | TUnknown => s"@NOT(${right.toNeko(ctx)})"
							case _ => s"$$not(${right.toNeko(ctx)})"
						}
						case "+" => Checker.tryGetType(ctx, right) match {
							case TNull | TString | TBool | TObject | TArray(_)
								| TFunction(_, _) | TAbstract(_) | TUnknown | TUnion(_) => s"@POS(${right.toNeko(ctx)})"
							case TInt | TFloat => right.toNeko(ctx)
							case TInvalid => scala.sys.error("Invalid type!")
						}
						case "-" => Checker.tryGetType(ctx, right) match {
							case TNull | TString | TBool | TObject | TArray(_)
								| TFunction(_, _) | TAbstract(_) | TUnknown | TUnion(_) => s"@NEG(${right.toNeko(ctx)})"
							case TInt | TFloat => "-"+right.toNeko(ctx)
							case TInvalid => scala.sys.error("Invalid type!")
						}
						case "~" => s"@BITNOT(${right.toNeko(ctx)})"
						case _ => scala.sys.error(s"Unknown operator `$symbol`!")
					}
				}
			}
		}
		
		
		// Message calls:
		sealed trait Message extends Expr { val sender: Expr }
		object Message {
			case class Single(sender: Expr, name: String) extends Message {
				def toNeko(ctx: Context) = send(sender.toNeko(ctx), name)
			}
			case class Multi(sender: Expr, params: List[(String, Expr)]) extends Message {
				def toNeko(ctx: Context): String = {
					val (names, vals) = params.unzip
					
					return send(sender.toNeko(ctx), names, vals.map(_.toNeko(ctx)))
				}
			}
			
			def send(sender: String, name: String) = s"$sender.@@send(${singleSel(name)},$$array())"
			def send(sender: String, args: List[(String, String)]): String = {
				val (names, vals) = args.unzip
				
				return send(sender, names, vals)
			}
			def send(sender: String, names: List[String], args: List[String]) = s"$sender.@@send(${multiSel(names)}," + array(args) + ")"
		}
		
		
		// Statement expressions:
		case class If(cond: Expr, thenStmt: Statement) extends Expr {
			def toNeko(ctx: Context) = s"if ${truthy(ctx, cond)} ${thenStmt.toNeko(ctx)}"
		}
		case class IfElse(cond: Expr, thenStmt: Statement, elseStmt: Statement) extends Expr {
			def toNeko(ctx: Context) = s"if ${truthy(ctx, cond)} ${thenStmt.toNeko(ctx)} else ${elseStmt.toNeko(ctx)}"
		}
		
		case class While(cond: Expr, stmt: Statement) extends Expr {
			def toNeko(ctx: Context) = s"while ${truthy(ctx, cond)} ${stmt.toNeko(ctx)}"
		}
		
		case class DoWhile(stmt: Statement, cond: Expr) extends Expr {
			def toNeko(ctx: Context) = s"do ${stmt.toNeko(ctx)} while ${truthy(ctx, cond)}"
		}
		
		case class For(decl: Option[Statement], cond: Option[Expr], update: Option[Expr], stmt: Statement) extends Expr {
			def toNeko(ctx: Context): String = {
				val loop = update match {
					case Some(u) => DoWhile(
						cond match {
							case Some(c) => Block({
								val check = If(
									Call(Builtin("$not"), List(Truthy(c))),
									Statement.Break(None)
								)
								
								stmt match {
									case Block(stmts) => check :: stmts
									case _ => List(check, stmt)
								}
							})
							case None => stmt
						},
						Op.Infix(Paren(u), "||", NekoValue.Bool(true))
					)
					case None => While(
						cond match {
							case Some(c) => Truthy(c)
							case None => NekoValue.Bool(true)
						},
						stmt
					)
				}
				
				val res = decl match {
					case Some(d) => Block(List(d, loop))
					case None => loop
				}
				
				res.toNeko(ctx)
			}
		}
		
		case class ForXInY(xIsNew: Boolean, x: String, y: Expr, stmt: Statement) extends Expr {
			def toNeko(ctx: Context) = ctx.genSym {e =>
				String.join("",
					"{var ",
					(if(xIsNew) s"$x," else ""),
					s"$e=ON_MakeEnumerator(${y.toNeko(ctx)});\n",
					s"while ($x=${Message.send(e, "nextValue")})!=${Message.send("ON_Nil", "make")} ",
					stmt.toNeko(ctx),
					"}"
				)
			}
		}
		
		case class ForXYInZ(xyAreNew: Boolean, x: String, y: String, z: Expr, stmt: Statement) extends Expr {
			def toNeko(ctx: Context) = ctx.genSym {e =>
				String.join("",
					"{var ",
					(if(xyAreNew) s"$x,$y," else ""),
					s"$e=ON_MakeEnumerator2(${z.toNeko(ctx)});\n",
					s"while {$y=${Message.send(e+"[1]", "nextValue")};$x=${Message.send(e+"[0]", "nextValue")}}!=${Message.send("ON_Nil", "make")} ",
					stmt.toNeko(ctx),
					"}"
				)
			}
		}
		
		case class TryCatch(tryStmt: Statement, name: String, catchStmt: Statement) extends Expr {
			def toNeko(ctx: Context) = s"try ${tryStmt.toNeko(ctx)} catch $name ${catchStmt.toNeko(ctx.inner(name -> Checker.TUnknown))}"
		}
		
		sealed trait SwitchCase { def toNeko(ctx: Context): String }
		object SwitchCase {
			case class Case(value: Expr, stmt: Statement) extends SwitchCase {
				def toNeko(ctx: Context) = value.toNeko(ctx) + "=>" + stmt.toNeko(ctx)
			}
			case class Default(stmt: Statement) extends SwitchCase {
				def toNeko(ctx: Context) = "default=>" + stmt.toNeko(ctx)
			}
		}
		case class Switch(target: Expr, cases: List[SwitchCase]) extends Expr {
			def toNeko(ctx: Context) = String.join("",
				"switch ",
				target.toNeko(ctx),
				cases.map(_.toNeko(ctx)).mkString("{", "\n", "}")
			)
		}
		
		
		// Misc:
		case class Truthy(expr: Expr) extends Expr { def toNeko(ctx: Context) = truthy(ctx, expr) }
		case class Raw(code: String) extends Expr { def toNeko(ctx: Context) = this.code }
	}
	
	sealed trait Statement {
		import Statement._
		
		def toNeko(ctx: Context): String
		def toObjn(tabs: String = ""): String = this match {
			case VarDecl(decls) =>
				decls match {
					case List((name, value)) => ' ' + name + value.map(v => " = " + v.toObjn(tabs)).getOrElse("")
					case _ =>
						val tabs2 = tabs + TAB
						decls.map {
							case (name, None) => '\n' + tabs2 + name
							case (name, Some(value)) => '\n' + tabs2 + name + " = " + value.toObjn(tabs2)
						}.mkString("var", ",", "")
				}
			
			case _ => scala.sys.error("idk")
		}
	}
	object Statement {
		case class VarDecl(decls: List[(String, Option[Expr])]) extends Statement {
			def toNeko(ctx: Context) =
				"var " + decls.map {
					case (name, Some(value)) => {
						ctx.add(name, Checker.tryGetType(ctx, value))
						s"$name=${value.toNeko(ctx)}"
					}
					case (name, None) => {
						ctx.add(name, Checker.TNull)
						name
					}
				}.mkString(",\n\t")
		}
		
		// FIX
		case class FuncDecl(name: String, args: Expr.FuncArgs, body: Expr.Block) extends Statement {
			def toNeko(ctx: Context) =
				s"$name=" + (this.args match {
					case Expr.FuncArgs.Args(List()) => s"(function()${this.body.toNeko(ctx)})"
					case Expr.FuncArgs.Args(args) if args.forall(_._1.isEmpty) => s"(function(${args.map(_._2).mkString(",")})${this.body.toNeko(ctx)})"
					case Expr.FuncArgs.Args(args) => {
						val checks = args.flatMap {
							case (Some(t), name) => Some(s"objn_Typecheck(${t.toNeko},$name,false);")
							case _ => None
						}.mkString
						
						s"(function(${args.map(_._2).mkString(",")})${this.body.toNeko(ctx, checks)})"
					}
					case Expr.FuncArgs.Varargs(arg) => s"$$varargs(function($arg)${this.body.toNeko(ctx)})"
				})
		}
		
		sealed trait MethodKind { def toNeko: String }
		object MethodKind {
			case object Klass extends MethodKind { def toNeko = "class" }
			case object Instance extends MethodKind { def toNeko = "instance" }
		}
		
		sealed trait InterfaceBody { def toNeko(ctx: Context, klass: String): String }
		object InterfaceBody {
			case class Property(name: String, attrs: List[String], value: Option[Expr]) extends InterfaceBody {
				def toNeko(ctx: Context, klass: String): String = {
					val n = '"' + name + '"'
					val a = if(attrs.isEmpty) "null" else attrs.map(_ + "=>true").mkString("{", ",", "}")
					val v = value match {
						case None => "null"
						case Some(v) => s"function(){${v.toNeko(ctx)}}"
					}
					
					return s"$klass.@@add_var($n,$a,$v)"
				}
			}
			case class SingleMethod(kind: MethodKind, name: String) extends InterfaceBody {
				def toNeko(ctx: Context, klass: String) = s"$klass.@@add_${kind.toNeko}_method(${singleSel(name)})"
			}
			case class MultiMethod(kind: MethodKind, args: List[(String, String)]) extends InterfaceBody {
				def toNeko(ctx: Context, klass: String) = s"$klass.@@add_${kind.toNeko}_method(${multiSel(args.unzip._1)})"
			}
		}
		case class Interface(name: String, parent: Option[String], body: List[InterfaceBody]) extends Statement {
			def toNeko(ctx: Context): String = {
				val p = parent match {
					case None => "null"
					case Some(p) => p
				}
				val b = body match {
					case List() => ""
					case _ => body.map(_.toNeko(ctx, name)).mkString("\n", ";\n", ";\n")
				}
				
				return s"var $name=@class.new(" + '"' + name + '"' + s",$p);$b"
			}
		}
		
		sealed trait ImplementationBody { def toNeko(ctx: Context, klass: String): String }
		object ImplementationBody {
			case class SingleMethod(kind: MethodKind, name: String, body: Expr.Block) extends ImplementationBody {
				def toNeko(ctx: Context, klass: String) =
					s"$klass.@@${kind.toNeko}_method(${singleSel(name)},function()" + body.toNeko(ctx) + ");"
			}
			case class MultiMethod(kind: MethodKind, args: List[(String, Option[Expr.Type], String)], body: Expr.Block) extends ImplementationBody {
				def toNeko(ctx: Context, klass: String) = {
					val (labels, _, names) = args.unzip3
					
					String.join("",
						s"$klass.@@${kind.toNeko}_method(${multiSel(labels)},function(${names.mkString(",")})",
						body.toNeko(ctx, args.flatMap {
							case (_, None, n) => None
							case (_, Some(t), n) => Some(s"objn_Typecheck(${t.toNeko},$n,false);")
						}.mkString),
						");"
					)
				}
			}
		}
		case class Implementation(name: String, body: List[ImplementationBody]) extends Statement {
			def toNeko(ctx: Context): String = s"$name.@@implementation();" + body.map(_.toNeko(ctx, name) + "\n").mkString
		}
		
		case class Return(value: Option[Expr]) extends Statement {
			def toNeko(ctx: Context) = value match {
				case Some(v) => "return " + v.toNeko(ctx)
				case None => "return;"
			}
		}
		
		case class Break(value: Option[Expr]) extends Statement {
			def toNeko(ctx: Context) = value match {
				case Some(v) => "break " + v.toNeko(ctx)
				case None => "break;"
			}
		}
		
		case object Continue extends Statement { def toNeko(ctx: Context) = "continue" }
		
		case class ImportLib(libName: String, alreadyImported: Boolean) extends Statement {
			def toNeko(ctx: Context) = if(alreadyImported) {
				""
			} else {
				s"\n/* <$libName> */\n" + ctx.imports(libName)
			}
		}
		case class ImportFile(filePath: String, alreadyImported: Boolean) extends Statement {
			def toNeko(ctx: Context) = if(alreadyImported) {
				""
			} else {
				s"\n/* $filePath */\n" + ctx.imports(filePath)
			}
		}
	}
}