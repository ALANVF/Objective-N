import scala.util.parsing.combinator._
import sys.process._
import scala.io.Source
import java.io._
import objn.Ast.{Context, Expr, Statement}

object Main extends App {
	val ctx = new Context()
	
	class ONParser extends RegexParsers with PackratParsers {
		def keyword:  Parser[Expr] = "var|if|else|do|while|for|return|break|continue|switch|case|default|function|try|catch".r ^^ Expr.Keyword.apply
		def name:     Parser[String] = "[a-zA-Z_]\\w*".r
		def variable: Parser[String] = not(neko_value | objn_value | keyword) ~> name
		def builtin:  Parser[Expr] = "\\$\\w+".r ^^ Expr.Builtin.apply
		
		def validType: Parser[Expr.Type] = rep1sep(
			(
				(
					"$tnull"     |
					"$tint"      |
					"$tfloat"    |
					"$tbool"     |
					"$tstring"   |
					"$tobject"   |
					"$tarray"    |
					"$tfunction" |
					"$tabstract"
				) ^^ Expr.Type.Builtin.apply
					|
				rep1sep(name, ".") ^^ Expr.Type.Compound.apply
			),
			"|"
		) ^^ {
			case List(t) => t
			case ts => Expr.Type.Union(ts)
		}
		
		def neko_value: Parser[Expr] = (
			raw"""(?:"(?:\\\\||\\"||[^"])*?")""".r ^^ { s => Expr.NekoValue.String(s.tail.init) }
				|
			"\\d+\\.\\d+".r ^^ { f => Expr.NekoValue.Float(f.toDouble) }
				|
			"\\d+".r ^^ { i => Expr.NekoValue.Int(i.toInt) }
				|
			("true" | "false") ^^ { b => Expr.NekoValue.Bool(b.toBoolean) }
				|
			"null" ^^^ Expr.NekoValue.Null
				|
			"this" <~ not("->") ^^^ Expr.NekoValue.This
				|
			"{" ~> rep1sep((name <~ "=>") ~ expr ^^ {case k ~ v => (k, v)}, ",") <~ "}" ^^ Expr.NekoValue.Object.apply
		)

		def objn_value: Parser[Expr] = (
			"nil" ^^^ Expr.ObjNValue.Nil
				|
			"NULL" ^^^ Expr.ObjNValue.Null
				|
			raw"""(?:@"(?:\\\\||\\"||[^"])*?")""".r ^^ { s => Expr.ObjNValue.String(s.drop(2).init) }
				|
			"@\\d+\\.\\d+".r ^^ { f => Expr.ObjNValue.Float(f.tail.toDouble) }
				|
			"@\\d+".r ^^ { i => Expr.ObjNValue.Int(i.tail.toInt) }
		)

		def objn_array: Parser[Expr] = "@[" ~> repsep(expr, ",") <~ "]" ^^ Expr.ObjNValue.Array.apply
		
		def objn_dict: Parser[Expr] = "@{" ~> repsep((expr <~ ":") ~ expr ^^ {case k ~ v => (k, v)}, ",") <~ "}" ^^ Expr.ObjNValue.Dict.apply
		
		def objn_box: Parser[Expr] = "@(" ~> expr <~ ")" ^^ Expr.Box.apply
		
		def func: Parser[Expr] = (
			("^" | "function") ~> (
				"(" ~> (
					repsep((validType <~ guard(name)).? ~ name ^^ {case t ~ n => (t, n)}, ",") ^^ Expr.FuncArgs.Args.apply
						|||
					(name <~ "...") ^^ Expr.FuncArgs.Varargs.apply
				) <~ ")"
			).? ~ block
		) ^^ {
			case None ~ b => Expr.Func(Expr.FuncArgs.Args(List()), b)
			case Some(a) ~ b => Expr.Func(a, b)
		}

		def message: Parser[Expr] =
			("[" ~> expr) ~ (
				(("[a-zA-Z_]\\w*".r) ~ (":" ~> expr) ^^ {case k ~ v => (k, v)}).+ ^^ Right.apply
					|
				("[a-zA-Z_]\\w*".r) ^^ Left.apply
			) <~ "]" ^^ {
				case sender ~ msg => msg match {
					case Left(msg)                         => Expr.Message.Single(sender, msg)
					case Right(args: List[(String, Expr)]) => Expr.Message.Multi(sender, args)
				}
			}
			
		def selectorWithName: Parser[Expr.Selector] = name ^^ Expr.Selector.Single.apply
		def selectorWithNames: Parser[Expr.Selector] = (name <~ ":").+ ^^ Expr.Selector.Multi.apply

		def selector: Parser[Expr] = "@selector(" ~> (selectorWithName ||| selectorWithNames) <~ ")"
		
		def block: Parser[Expr.Block] = "{" ~> program <~ "}" ^^ Expr.Block.apply

		//ops:
		def op9: Parser[Expr] =
			op10 ~ (
				("(" ~> repsep(expr, ",") <~ ")" ^^ { p => (Some(p), None, None) })
					|||
				("[" ~> expr <~ "]" ^^ { i => (None, Some(i), None) })
					|||
				("." ~> name ^^ { f => (None, None, Some(f)) })
			).* ^^ {
				case left ~ List() => left
				case left ~ right => right.foldLeft(left) {
					case (l, (Some(params), None, None)) => Expr.Call(l, params)
					case (l, (None, Some(index), None)) => Expr.GetIndex(l, index)
					case (l, (None, None, Some(field))) => Expr.GetField(l, field)
				}
			}

		def op8: Parser[Expr] =
			("!" | "+" | "-" | "~").? ~ op9 ^^ {
				case Some(sym) ~ right => Expr.Op.Prefix(sym, right)
				case None      ~ right => right
			}

		def op7: Parser[Expr] =
			rep1sep(op8, "**") ^^ { _.reduceLeft(Expr.Op.Infix(_, "**", _)) }

		def op6: Parser[Expr] =
			op7 ~ (("*" | "/" | "%") ~ op7 ^^ {case o ~ r => (o, r)}).* ^^ {
				case left ~ List() => left
				case left ~ right => right.foldLeft(left) {case (l, (o, r)) => Expr.Op.Infix(l, o, r)}
			}

		def op5: Parser[Expr] =
			op6 ~ (("+" | "-") ~ op6 ^^ {case o ~ r => (o, r)}).* ^^ {
				case left ~ List() => left
				case left ~ right => right.foldLeft(left) {case (l, (o, r)) => Expr.Op.Infix(l, o, r)}
			}

		def op4: Parser[Expr] =
			op5 ~ (("<<" | ">>" | "&" | "|" | "^") ~ op5 ^^ {case o ~ r => (o, r)}).* ^^ {
				case left ~ List() => left
				case left ~ right => right.foldLeft(left) {case (l, (o, r)) => Expr.Op.Infix(l, o, r)}
			}

		def op3: Parser[Expr] =
			op4 ~ (("==" | "!=" | ">=" | "<=" | ">" | "<") ~ op4 ^^ {case o ~ r => (o, r)}).* ^^ {
				case left ~ List() => left
				case left ~ right => right.foldLeft(left) {case (l, (o, r)) => Expr.Op.Infix(l, o, r)}
			}

		def op2: Parser[Expr] =
			op3 ~ (("&&" | "||") ~ op3 ^^ {case o ~ r => (o, r)}).* ^^ {
				case left ~ List() => left
				case left ~ right => right.foldLeft(left) {case (l, (o, r)) => Expr.Op.Infix(l, o, r)}
			}

		lazy val expr: PackratParser[Expr] =
			op2 ~ (
				("=" ~> op2).+ ^^ Left.apply
					|
				("+=" | "-="  | "++=" | "--=" | "*=" | "/=" | "%=" | "**=" | "<<=" | ">>=" | "&=" | "|=" | "^=" | "&&=" | "||=") ~ op2 ^^ Right.apply
			).? ^^ {
				case left ~ None => left
				case left ~ Some(Left(right: List[Expr])) => right.foldLeft(left) {case (l, r) => Expr.Op.Infix(l, "=", r)}
				case left ~ Some(Right(sym ~ right)) => Expr.Op.Infix(left, sym, right)
			}

		//expr:
		lazy val if_expr: PackratParser[Expr] =
			("if" ~> expr) ~ statement ~ ("else" ~> statement).? ^^ {
				case c ~ b1 ~ Some(b2) => Expr.IfElse(c, b1, b2)
				case c ~ b1 ~ None     => Expr.If(c, b1)
			}

		def do_while_expr: Parser[Expr] =
			("do" ~> statement) ~ ("while" ~> expr) ^^ {case b ~ c => Expr.DoWhile(b, c)}

		def while_expr: Parser[Expr] =
			("while" ~> expr) ~ statement ^^ {case c ~ b => Expr.While(c, b)}

		def for_expr: Parser[Expr] =
			("for" ~ "(") ~> (statement.? <~ ";") ~ (expr.? <~ ";") ~ (expr.? <~ ")") ~ statement ^^ {
				case e1 ~ e2 ~ e3 ~ b => Expr.For(e1, e2, e3, b)
			}
		
		def for_in_expr: Parser[Expr] =
			("for" ~ "(") ~> "var".? ~ name ~ ("," ~> name).? ~ ("in" ~> expr <~ ")") ~ statement ^^ {
				case Some(_) ~ v1 ~ Some(v2) ~ e ~ b => Expr.ForXYInZ(true, v1, v2, e, b)
				case Some(_) ~ v1 ~ None     ~ e ~ b => Expr.ForXInY(true, v1, e, b)
				case None    ~ v1 ~ Some(v2) ~ e ~ b => Expr.ForXYInZ(false, v1, v2, e, b)
				case None    ~ v1 ~ None     ~ e ~ b => Expr.ForXInY(false, v1, e, b)
			}
		
		def try_catch_expr: Parser[Expr] =
			("try" ~> statement) ~ ("catch" ~> name) ~ statement ^^ {
				case t ~ e ~ b => Expr.TryCatch(t, e, b)
			}

		def switch_expr: Parser[Expr] =
			("switch" ~> expr) ~ ("{" ~> (
				("case" ~> expr <~ ":") ~ statement ^^ {case e ~ s => Some(Expr.SwitchCase.Case(e, s))}
					|
				("default" <~ (":" | "=>")) ~> statement ^^ {s => Some(Expr.SwitchCase.Default(s))}
					|
				(expr <~ "=>") ~ statement  ^^ {case e ~ s => Some(Expr.SwitchCase.Case(e, s))}
					|
				lineComment ^^^ None
			).* <~ "}") ^^ {
				case e ~ b => Expr.Switch(e, b.flatten)
			}

		lazy val op10: PackratParser[Expr] = (
			("this" ~ "->") ~> name ^^ Expr.GetMember.apply /* fix this eventually */
				|
			neko_value
				|
			objn_value
				|
			func
				|
			objn_array
				|
			objn_dict
				|
			objn_box
				|
			selector
				|
			builtin
				|
			variable ^^ Expr.Variable.apply
				|
			message
				|
			block
				|
			if_expr
				|
			do_while_expr
				|
			while_expr
				|
			for_expr
				|
			for_in_expr
				|
			try_catch_expr
				|
			switch_expr
				|
			not(expr) ~> "(" ~> expr <~ ")" ^^ Expr.Paren.apply
				|
			expr
		)

		//objn statements:
		def objn_method_kind: Parser[Statement.MethodKind] = (
			"+" ^^^ Statement.MethodKind.Klass
				|
			"-" ^^^ Statement.MethodKind.Instance
		)
		
		def objn_interface: Parser[Statement] =
			(
				("@interface" ~> name) ~ (":" ~> name).? ~ (
					objn_method_kind ~ (
						name ^^ Left.apply
							|||
						((name <~ ":") ~ name ^^ {case l ~ r => (l, r)}).+ ^^ Right.apply
					) ^^ {
						case k ~ Left(n) => Some(Statement.InterfaceBody.SingleMethod(k, n))
						case k ~ Right(args) => Some(Statement.InterfaceBody.MultiMethod(k, args))
					}
					
						|
					"@property" ~> name ~ ("(" ~> rep1sep(name, ",") <~ ")").? ~ ("=" ~> expr <~ ";").? ^^ {
						case n ~ None ~ v => Some(Statement.InterfaceBody.Property(n, List(), v))
						case n ~ Some(a) ~ v => Some(Statement.InterfaceBody.Property(n, a, v))
					}
						|
					lineComment ^^^ None
				).* <~ "@end"
			) ^^ {
				case n ~ p ~ b => Statement.Interface(n, p, b.flatten)
			}

		def objn_implementation: Parser[Statement] =
			(
				("@implementation" ~> name) /*~ ("(" ~> rep1sep(name, ",") <~ ")").?*/ ~ (
					objn_method_kind ~ (
						name ^^ Left.apply
							|||
						((name <~ ":") ~ ("(" ~> validType <~ ")").? ~ name ^^ {case l ~ t ~ r => (l, t, r)}).+ ^^ Right.apply
					) ~ block ^^ {
						case k ~ Left(n) ~ b => Some(Statement.ImplementationBody.SingleMethod(k, n, b))
						case k ~ Right(args) ~ b => Some(Statement.ImplementationBody.MultiMethod(k, args, b))
					}
						|
					lineComment ^^^ None
				).* <~ "@end"
			) ^^ {
				case n ~ body => Statement.Implementation(n, body.flatten)
			}

		//statements:
		def var_decl: Parser[Statement] =
			"var" ~> rep1sep(name ~ ("=" ~> expr).? ^^ {case n ~ v => (n, v)}, ",") ^^ Statement.VarDecl.apply

		def return_stmt: Parser[Statement] = "return" ~> expr.? ^^ Statement.Return.apply

		def break_stmt: Parser[Statement] = "break" ~> expr.? ^^ Statement.Break.apply

		def continue_stmt: Parser[Statement] = "continue" ^^^ Statement.Continue

		def func_stmt: Parser[Statement] = (
			("function" ~> name) ~ (
				"(" ~> (
					repsep((validType <~ guard(name)).? ~ name ^^ {case t ~ n => (t, n)}, ",") ^^ Expr.FuncArgs.Args.apply
						|||
					(name <~ "...") ^^ Expr.FuncArgs.Varargs.apply
				) <~ ")"
			) ~ block
		) ^^ {
			case n ~ a ~ b => Statement.FuncDecl(n, a, b)
		}

		def import_macro_stmt: Parser[Statement] =
			"#import" ~> ("\"(?:\\\"|[^\"])*?\"".r) ^^ {e =>
				val filePath = s"$e".init.tail
				
				if(ctx.imports.contains(filePath)) {
					Statement.ImportFile(filePath, true)
				} else {
					ctx.imports(filePath) = if(filePath endsWith ".neko") {
						Source.fromFile(filePath).mkString
					} else if((filePath endsWith ".mn") || (filePath endsWith ".hn")) {
						ctx.imports(filePath) = "" // To prevent circular imports
						parser(Source.fromFile(filePath).mkString)
					} else {
						scala.sys.error("Invalid file import \"" + filePath + "\"!")
					}
					
					Statement.ImportFile(filePath, false)
				}
			}

		def statement: Parser[Statement] = (
			(objn_interface
				|||
			objn_implementation
				|||
			var_decl
				|||
			return_stmt
				|||
			break_stmt
				|||
			continue_stmt
				|||
			func_stmt
				|||
			import_macro_stmt)
				|
			expr
		)

		def lineComment: Parser[String] = "//[^\\n\\r]*".r
		def program: Parser[List[Statement]] = (
			lineComment ^^ {c => Expr.Raw(c + "\n")}
				|||
			statement <~ ";".?
		).*

		// parse thing
		def apply(input: String): String = {
			parseAll(program, input) match {
				case Success(result, _) => result.map(_.toNeko(ctx)).mkString(";\n")
				case failure: NoSuccess => scala.sys.error(failure.toString)
			}
		}
	}

	val parser = new ONParser
	val flag = "^\\-.*".r
	
	args(0) match {
		case "-h" | "--help" => println("Usage: `objn-parser [options] filename` (you don't need the file extension).\n\n" +
			"-h, --help | Display this message.\n" +
			"-c         | Compile to Neko only.\n" +
			"-b         | Compile to Neko Bytecode only.")
		
		case "-c" => {
			val sourceFile = Source.fromFile(args(1) + ".mn").mkString
			var output = s"// ${args(1)}.mn" + "\n\n" + parser(sourceFile)
			
			new PrintWriter(args(1) + ".neko") {write(output); close}
		}
		
		case "-b" => {
			val sourceFile = Source.fromFile(args(1) + ".mn").mkString
			var output = s"// ${args(1)}.mn" + "\n\n" + parser(sourceFile)
			
			new PrintWriter(args(1) + ".neko") {write(output); close}
			
			s"nekoc ${args(1)}.neko".!
		}
		
		case flag(_*) => println(s"Unknown flag `${args(0)}`. Try -h or --help to see all options.")
		
		case _ => {
			val sourceFile = Source.fromFile(args(0) + ".mn").mkString
			var output = s"// ${args(0)}.mn\n\n" + parser(sourceFile)
			
			new PrintWriter(args(0) + ".neko") {write(output); close}
			
			s"nekoc ${args(0)}.neko".!
			s"neko ${args(0)}.n ${args.tail.mkString(" ")}".!
		}
	}
}
