import scala.util.parsing.combinator._
import sys.process._
import scala.io.Source
import java.io._
import objn.Ast.{Context, Expr, Statement}
import objn.Checker

object Main extends App {
	val ctx = new Context()
	
	class ONParser extends RegexParsers with PackratParsers {
		def keyword:  Parser[Expr] = "var|if|else|do|while|for|return|break|continue|switch|case|default|function|try|catch".r ^^ Expr.Keyword.apply
		def name:     Parser[String] = "[a-zA-Z_]\\w*".r
		def variable: Parser[String] = not(neko_value | objn_value | keyword) ~> name
		def builtin:  Parser[Expr] = "\\$\\w+".r ^^ Expr.Builtin.apply
		
		def basicValidType: Parser[Expr.Type] = (
			"_" ^^^ Expr.Type.Dynamic
				|
			(
				"$tnull"                 |
				"$tint"                  |
				"$tfloat"                |
				"$tbool"                 |
				"$tstring"               |
				"$tobject"               |
				"$tarray"    <~ not("(") |
				"$tfunction" <~ not("(") |
				"$tabstract" <~ not("(")
			) ^^ Expr.Type.Builtin.apply
				|
			rep1sep(name, ".") ^^ Expr.Type.Compound.apply
				|
			(
				"$tarray(" ~> validType <~ ")" |
				"[" ~> validType <~ "]"
			) ^^ Expr.Type.Array.apply
				|
			(("$tfunction" | "^(") ~> (
				"..." ^^^ None |
				repsep(validType, ",") ^^ Some.apply
			)) ~ ("):" ~> basicValidType) ^^ {
				case params ~ ret => Expr.Type.Func(params, ret)
			}
				|
			"$tabstract(" ~> name <~ ")" ^^ Expr.Type.Abstract.apply
		)
		
		def validType: Parser[Expr.Type] = rep1sep(basicValidType, "|") ^^ {
			case List(t) => t
			case ts => Expr.Type.Union(ts)
		}
		
		def neko_value: Parser[Expr] = (
			raw"""(?:"(?:\\x[a-fA-F\d]{2}|\\.|[^\\"]+)*?")""".r ^^ { s => Expr.NekoValue.String(s.tail.init) }
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
			raw"""(?:@"(?:\\x[a-fA-F\d]{2}|\\.|[^\\"]+)*?")""".r ^^ { s => Expr.ObjNValue.String(s.drop(2).init) }
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
			).? ~ (":" ~> validType).? ~ block
		) ^^ {
			case None ~ ret ~ b => Expr.Func(Expr.FuncArgs.Args(List()), ret, b)
			case Some(a) ~ ret ~ b => Expr.Func(a, ret, b)
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
				(lineComment | multilineComment) ^^^ None
			).* <~ "}") ^^ {
				case e ~ b => Expr.Switch(e, b.flatten)
			}
		
		def raw_expr: Parser[Expr] = "(?ms)`[^`]+`".r ^^ { code => Expr.Raw(code.init.tail) }

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
			raw_expr
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
					"@property" ~> ("(" ~> rep1sep(name, ",") <~ ")").? ~ name ~ (":" ~> validType).? ~ ("=" ~> expr <~ ";").? ^^ {
						case None ~ n ~ t ~ v => Some(Statement.InterfaceBody.Property(List(), n, t, v))
						case Some(a) ~ n ~ t ~ v => Some(Statement.InterfaceBody.Property(a, n, t, v))
					}
						|
					(lineComment | multilineComment) ^^^ None
				).* <~ "@end"
			) ^^ {
				case n ~ p ~ b => Statement.Interface(n, p, b.flatten)
			}

		def objn_implementation: Parser[Statement] =
			(
				("@implementation" ~> name) /*~ ("(" ~> rep1sep(name, ",") <~ ")").?*/ ~ (
					objn_method_kind ~ ("(" ~> validType <~ ")").? ~ (
						name ^^ Left.apply
							|||
						((name <~ ":") ~ ("(" ~> validType <~ ")").? ~ name ^^ {case l ~ t ~ r => (l, t, r)}).+ ^^ Right.apply
					) ~ block ^^ {
						case k ~ t ~ Left(n) ~ b => Some(Statement.ImplementationBody.SingleMethod(k, t, n, b))
						case k ~ t ~ Right(args) ~ b => Some(Statement.ImplementationBody.MultiMethod(k, t, args, b))
					}
						|
					(lineComment | multilineComment) ^^^ None
				).* <~ "@end"
			) ^^ {
				case n ~ body => Statement.Implementation(n, body.flatten)
			}

		//statements:
		def var_decl: Parser[Statement] =
			"var" ~> rep1sep(name ~ (":" ~> validType).? ~ ("=" ~> expr).? ^^ {case n ~ t ~ v => (n, t, v)}, ",") ^^ Statement.VarDecl.apply

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
			) ~ (":" ~> validType).? ~ block
		) ^^ {
			case n ~ a ~ ret ~ b => Statement.FuncDecl(n, a, ret, b)
		}

		def import_macro_stmt: Parser[Statement] =
			"#import" ~> ("\"(?:\\.|[^\\\"])*?\"".r) ^^ {e =>
				val filePath = s"$e".init.tail
				
				if(ctx.imports.contains(filePath)) {
					Statement.ImportFile(filePath, true)
				} else {
					ctx.imports(filePath) = if(filePath endsWith ".neko") {
						import objn.Checker._
						if(filePath endsWith "/objn/object.neko") {
							ctx.add("ON_Object", TObject)
							ctx.add("id", TObject)
							ctx.add(
								"objn_Typecheck",
								TFunction(
									Some(List(
										TInt | TObject | TArray(TInt | TObject),
										TUnknown,
										TBool
									)),
									TBool | TNull
								)
							)
							//ctx.add("objn_Copy", $tfunction@<T>(T): T)
						}
						
						Source.fromFile(filePath).mkString
					} else if((filePath endsWith ".mn") || (filePath endsWith ".hn")) {
						ctx.imports(filePath) = "" // To prevent circular imports
						try {
							parser(Source.fromFile(filePath).mkString)
						} catch {
							case e: java.lang.RuntimeException =>
								throw new java.lang.RuntimeException(s"Error parsing file `$filePath`: ${e.getMessage()}", e)
						}
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
		def multilineComment: Parser[String] = "(?ms)/\\*.*?\\*/".r
		def program: Parser[List[Statement]] = (
			lineComment ^^ {c => Expr.Raw(c + "\n")}
				|||
			multilineComment ^^ {c => Expr.Raw(c)}
				|||
			statement <~ ";".?
		).*

		// parse thing
		def apply(input: String): String = {
			parseAll(program, input) match {
				case Success(result, _) =>
					Checker.quiet = true
					
					val funcDecls = result.collect {
						case Statement.FuncDecl(name, params, ret, body) =>
							val (newCtx, paramTypes) = Checker.typeFuncParams(ctx, params)
							val retType = ret.map(Checker.Type.fromExprType(_))
							(name, params, retType, body, newCtx, paramTypes)
					}
					
					for((name, params, retType, body, newCtx, paramTypes) <- funcDecls) {
						ctx.add(name, Checker.TFunction(paramTypes, retType.getOrElse(Checker.TUnknown)))
					}
					
					for((name, params, retType, body, newCtx, paramTypes) <- funcDecls) {
						ctx.set(name, Checker.TFunction(paramTypes, {
							val inferred = Checker.funcRetType(newCtx.inner(), body)
							retType match {
								case Some(ret) =>
									/*if(!(ret accepts inferred)) {
										//Checker.quiet = false
										Checker.warn("warning: type `"+inferred.toObjn()+"` doesn't match specified return type `"+ret.toObjn()+"`")
										//Checker.quiet = true
									}*/
									ret
								case None => inferred
							}
						}))
					}
					
					Checker.quiet = false
					
					for(stmt <- result) {
						if(Checker.exprTyper.typeStmtWith(ctx, stmt) == Checker.TInvalid) {
							scala.sys.error("Invalid type for statement `"+stmt.toNeko(ctx)+"`") // TODO: change to toObjn
						}
					}
					
					result.map(_.toNeko(ctx)).mkString(";\n")
				case failure: NoSuccess => scala.sys.error(failure.toString)
			}
		}
	}

	val parser = new ONParser
	val flag = "^\\-.*".r
	val debug = true
	
	if(args.length == 0) {
		if(debug) {
			val path = "../tests/test5/main"
			val sourceFile = Source.fromFile(path+".mn").mkString
			var output = s"// $path.mn\n\n" + parser(sourceFile)
			
			new PrintWriter(path+".neko") {write(output); close}
			
			s"nekoc $path.neko".!
			s"neko $path.n".!
		} else {
			println("Use -h or --help to display a help message.")
		}
	} else {
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
}