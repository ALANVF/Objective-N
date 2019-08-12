import scala.util.parsing.combinator._
import sys.process._
import scala.io.Source
import java.io._
// TODO: fix continue in for loop
object Main extends App {
	def argsToSEL(args: List[Tuple2[Any, Any]]): String =
		"@SEL($array(" + (args map {t: (Any, Any) => "\""+(t._1)+"\""} mkString ",") + "),$array(" + (args map {t: (Any, Any) => "\""+(t._2)+"\""} mkString ",") + "))"

	def argsToMessage(args: List[Tuple2[Any, Any]]): String =
		"@SEL($array(" + (args map {t: (Any, Any) => "\""+(t._1)+"\""} mkString ",") + "),$array(" + (args map {t: (Any, Any) => ""+(t._2)} mkString ",") + "))"

	def messageS(caller: Any, name: String): String =
		s"$caller.@@send(@SEL(" + "$array(\""+name+"\"),$array()))"

	def messageM(caller: Any, args: List[Any]): String =
		s"$caller.@@send(${argsToMessage(args.asInstanceOf[List[(Any, Any)]])})"

	class ONParser extends RegexParsers with PackratParsers {
		def keyword: Parser[Any] = "var|if|else|do|while|for|return|break|continue|switch|case|default|function|try|catch".r

		def name: Parser[Any] = "[a-zA-Z_]\\w*".r
		def variable: Parser[Any] = not(neko_value | objn_value | keyword) ~> name
		def builtin: Parser[Any] = "\\$\\w+".r

		def neko_value: Parser[Any] = (
			raw""""(?:\\\\||\\"||[^"])*?"""".r
				|
			"\\d+\\.\\d+".r
				|
			"\\d+".r
				|
			("true" | "false")
				|
			"null"
				|
			"this" <~ not("->")
		)

		def objn_value: Parser[Any] = (
			"nil" ^^^ (messageS("ON_Nil", "new"))
				|
			"NULL" ^^^ (messageS("ON_Null", "make"))
				|
			raw"""@"(?:\\\\||\\"||[^"])*?"""".r ^^ {v=>messageM("ON_String", List(("stringWithNekoString", s"$v".tail)))}
				|
			"@\\d+\\.\\d+".r ^^ {v=>messageM("ON_Float", List(("floatWithNekoFloat", s"$v".tail)))}
				|
			"@\\d+".r ^^ {v=>messageM("ON_Integer", List(("integerWithNekoInteger", s"$v".tail)))}
		)

		def objn_array: Parser[Any] = "@[" ~> repsep(expr, ",") <~ "]" ^^ {
			case List()         => messageS("ON_Array", "array")
			case List(e)        => messageM("ON_Array", List(("arrayWithValue", e)))
			case (a: List[Any]) => messageM("ON_Array", List(("arrayWithValues", "$array("+(a mkString ",")+")")))
		}
		
		def objn_dict: Parser[Any] = "@{" ~> repsep((expr <~ ":") ~ expr ^^ {case k~v => Tuple2[Any, Any](k, v)}, ",") <~ "}" ^^ {
			case List() => messageS("ON_Dictionary", "new")
			case List((k, v)) => messageM("ON_Dictionary", List(("dictionaryWithValue", v), ("forKey", k)))
			// maybe use unzip here
			case (p: List[(Any, Any)]) => messageM("ON_Dictionary", List(
				("dictionaryWithValues", "$array("+(p map (_._2) mkString ",")+")"),
				("forKeys", "$array("+(p map (_._1) mkString ",")+")")
			))
		}
		
		def objn_box: Parser[Any] = "@(" ~> expr <~ ")" ^^ ("ON_BoxValue("+_+")")
		
		def func: Parser[Any] = ("^" ~> opt("(" ~> (repsep(name, ",") ||| (name <~ "...")) <~ ")") ~ block) ^^ {
			case None ~ b               => s"(function()$b)"
			case Some(List()) ~ b       => s"(function()$b)"
			case Some(a: List[Any]) ~ b => "(function(" + (a mkString ",") + s")$b)"
			case Some(a) ~ b            => "$varargs(function(" + s"$a)$b)"
		}

		def message: Parser[Any] = (
			("[" ~> expr) ~ (
				rep1(("[a-zA-Z_]\\w*".r) ~ (":" ~> expr) ^^ {case k~v => (k, v)})
					|
				("[a-zA-Z_]\\w*".r) ^^ (""+_)
			) <~ "]"
		) ^^ {
			case c ~ m => {
				m match {
					case a: List[Any] => messageM(s"$c", a)
					case _            => messageS(s"$c", s"$m")
				}
			}
		}

		def selectorWithName: Parser[Any] =
			name ^^ ("@SEL($array(\""+_+"\"),$array())")
		def selectorWithNames: Parser[Any] =
			rep1(name <~ ":") ^^ {mn => "@SEL($array(" + (mn map ("\""+_+"\"") mkString ",") + "),$array(" + ((List.fill(mn.length)("null")) mkString ",") + "))"}
		def selectorWithArgs: Parser[Any] =
			rep1(name ~ (":" ~> expr)) ^^ {l=> argsToMessage((l map {case m~v => (m, v)}).asInstanceOf[List[(Any, Any)]])}

		def selector: Parser[Any] = "@selector(" ~> (selectorWithName ||| selectorWithNames ||| selectorWithArgs) <~ ")"

		def block: Parser[Any] = "{" ~> program <~ "}" ^^ {b=> "{" + (b.asInstanceOf[List[Any]] mkString ";\n") + ";}"}

		//ops:
		def op9: Parser[Any] = op10 ~ (
			("(" ~> repsep(expr, ",") <~ ")" ^^ {case (a: List[Any]) => "("+(a mkString ",")+")"})
				|||
			("[" ~> expr <~ "]" ^^ ("["+_+"]"))
				|||
			("." ~> name ^^ ("."+_))
		).* ^^ {
			case l ~ List() => s"$l"
			case l ~ (r: List[Any]) => s"$l${r mkString}"
		}

		def op8: Parser[Any] = (("!" | "+" | "-" | "~") ^^ {
			case "!" => "@NOT"
			case "+" => "@POS"
			case "-" => "@NEG"
			case "~" => "@BITNOT"
		}).? ~ op9 ^^ {
			case Some(p) ~ r => s"$p($r)"
			case None ~ r    => s"$r"
		}

		def op7: Parser[Any] = rep1sep(op8, "**") ^^ {_ reduceLeft ("@POW("+_+","+_+")")}

		def op6: Parser[Any] = (
			/*rep1sep(op7, "*") ^^ {_ reduceLeft (""+_+"*"+_)}
				|||
			rep1sep(op7, "/") ^^ {_ reduceLeft (""+_+"/"+_)}
				|||
			rep1sep(op7, "%") ^^ {_ reduceLeft (""+_+"%"+_)}*/
			op7 ~ (("*" | "/" | "%") ~ op7 ^^ {case o~r => s"$o$r"}).* ^^ {
				case l ~ List() => s"$l"
				case l ~ (r: List[Any]) => s"$l" + (r mkString "")
			}
		)

		def op5: Parser[Any] = (
			/*rep1sep(op6, "+") ^^ {_ reduceLeft (""+_+"+"+_)}
				|||
			rep1sep(op6, "-") ^^ {_ reduceLeft (""+_+"-"+_)}*/
			op6 ~ (("+" | "-") ~ op6 ^^ {case o~r => s"$o$r"}).* ^^ {
				case l ~ List() => s"$l"
				case l ~ (r: List[Any]) => s"$l" + (r mkString "")
			}
		)

		def op4: Parser[Any] = (
			/*rep1sep(op5, "<<") ^^ {_ reduceLeft ("@SHL("+_+","+_+")")}
				|||
			rep1sep(op5, ">>") ^^ {_ reduceLeft ("@SHR("+_+","+_+")")}
				|||
			rep1sep(op5, "&") ^^ {_ reduceLeft ("@BITAND("+_+","+_+")")}
				|||
			rep1sep(op5, "|") ^^ {_ reduceLeft ("@BITOR("+_+","+_+")")}
				|||
			rep1sep(op5, "^") ^^ {_ reduceLeft ("@BITXOR("+_+","+_+")")}*/
			op5 ~ (("<<"^^^"@SHL" | ">>"^^^"@SHR" | "&"^^^"@BITAND" | "|"^^^"@BITOR" | "^"^^^"@BITXOR") ~ op5 ^^ {case o~r => Tuple2[Any, Any](o, r)}).* ^^ {
				case l ~ List() => s"$l"
				case l ~ (r: List[(Any, Any)]) => {
				 	var out = s"$l"
					r foreach {
						case (o, rv) => out = s"$o($out,$rv)"
					}
					out
				}
			}
		)

		def op3: Parser[Any] = (
			/*rep1sep(op4, "==") ^^ {_ reduceLeft (""+_+"=="+_)}
				|||
			rep1sep(op4, "!=") ^^ {_ reduceLeft (""+_+"!="+_)}
				|||
			rep1sep(op4, ">=") ^^ {_ reduceLeft (""+_+">="+_)}
				|||
			rep1sep(op4, "<=") ^^ {_ reduceLeft (""+_+"<="+_)}
				|||
			rep1sep(op4, ">") ^^ {_ reduceLeft (""+_+">"+_)}
				|||
			rep1sep(op4, "<") ^^ {_ reduceLeft (""+_+"<"+_)}*/
			op4 ~ (("==" | "!=" | ">=" | "<=" | ">" | "<") ~ op4 ^^ {case o~r => s"$o$r"}).* ^^ {
				case l ~ List() => s"$l"
				case l ~ (r: List[Any]) => s"$l" + (r mkString "")
			}
		)

		def op2: Parser[Any] = (
			rep1sep(op3, "&&") ^^ {_ reduceLeft ("@BOOL("+_+")&&@BOOL("+_+")")}
				|||
			rep1sep(op3, "||") ^^ {_ reduceLeft ("@BOOL("+_+")||@BOOL("+_+")")}
		)

		lazy val expr: PackratParser[Any] = (
			rep1sep(op2, "=")   ^^ {_ reduceLeft (""+_+"="+_)}  |||
			rep1sep(op2, "+=")  ^^ {_ reduceLeft (""+_+"+="+_)} |||
			rep1sep(op2, "-=")  ^^ {_ reduceLeft (""+_+"-="+_)} |||
			rep1sep(op2, "*=")  ^^ {_ reduceLeft (""+_+"*="+_)} |||
			rep1sep(op2, "/=")  ^^ {_ reduceLeft (""+_+"/="+_)} |||
			rep1sep(op2, "%=")  ^^ {_ reduceLeft (""+_+"%="+_)} |||
			rep1sep(op2, "**=") ^^ {_ reduceLeft {(l,r)=>""+l+"=@POW("+l+","+r+")"}} |||
			rep1sep(op2, "<<=") ^^ {_ reduceLeft {(l,r)=>""+l+"=@SHL("+l+","+r+")"}} |||
			rep1sep(op2, ">>=") ^^ {_ reduceLeft {(l,r)=>""+l+"=@SHR("+l+","+r+")"}} |||
			rep1sep(op2, "&=")  ^^ {_ reduceLeft {(l,r)=>""+l+"=@BITAND("+l+","+r+")"}} |||
			rep1sep(op2, "|=")  ^^ {_ reduceLeft {(l,r)=>""+l+"=@BITOR("+l+","+r+")"}}  |||
			rep1sep(op2, "^=")  ^^ {_ reduceLeft {(l,r)=>""+l+"=@BITXOR("+l+","+r+")"}} |||
			rep1sep(op2, "&&=") ^^ {_ reduceLeft (""+_+"&&="+_)} |||
			rep1sep(op2, "||=") ^^ {_ reduceLeft (""+_+"||="+_)}
		)

		//expr:
		lazy val if_expr: PackratParser[Any] =
			("if" ~> expr) ~ statement ~ opt("else" ~> statement) ^^ {
				case c ~ b1 ~ Some(b2) => s"if @BOOL($c) $b1 else $b2" /* make everything have $istrue() */
				case c ~ b1 ~ None     => s"if @BOOL($c) $b1"
			}

		def do_while_expr: Parser[Any] =
			("do" ~> statement) ~ ("while" ~> expr) ^^ {case b ~ c => s"do $b while @BOOL($c)"}

		def while_expr: Parser[Any] =
			("while" ~> expr) ~ statement ^^ {case c ~ b => s"while @BOOL($c) $b"}

		def for_expr: Parser[Any] =
			("for" ~ "(") ~> (opt(statement) <~ ";") ~ (opt(expr) <~ ";") ~ (opt(expr) <~ ")") ~ statement ^^ {
				case Some(e1) ~ Some(e2) ~ Some(e3) ~ b => s"{$e1;while @BOOL($e2) {$b;$e3}}"
				case None ~ Some(e2) ~ Some(e3) ~ b     => s"while @BOOL($e2) {$b;$e3}"
				case Some(e1) ~ None ~ Some(e3) ~ b     => s"{$e1;while true {$b;$e3}}"
				case Some(e1) ~ Some(e2) ~ None ~ b     => s"{$e1;while @BOOL($e2) $b}"
				case None ~ None ~ Some(e3) ~ b         => s"while true {$b;$e3}"
				case None ~ Some(e2) ~ None ~ b         => s"while @BOOL($e2) $b"
				case Some(e1) ~ None ~ None ~ b         => s"{$e1;while true $b}"
				case None ~ None ~ None ~ b             => s"while true $b"
			}
		
		def for_in_expr: Parser[Any] =
			("for" ~ "(") ~> "var".? ~ name ~ ("," ~> name).? ~ ("in" ~> expr <~ ")") ~ statement ^^ {
				case Some(_) ~ v1 ~ Some(v2) ~ e ~ b => s"""{var $v1,$v2,@__e=ON_MakeEnumerator2($e);while {$v2=${messageS("@__e[1]","nextValue")};$v1=${messageS("@__e[0]","nextValue")}}!=${messageS("ON_Nil","make")} $b}"""
				case Some(_) ~ v1 ~ None     ~ e ~ b => s"""{var $v1,@__e=ON_MakeEnumerator($e);while ($v1=${messageS("@__e","nextValue")})!=${messageS("ON_Nil","make")} $b}"""
				case None    ~ v1 ~ Some(v2) ~ e ~ b => s"""{var @__e=ON_MakeEnumerator2($e);while {$v2=${messageS("@__e[1]","nextValue")};$v1=${messageS("@__e[0]","nextValue")}}!=${messageS("ON_Nil","make")} $b}"""
				case None    ~ v1 ~ None     ~ e ~ b => s"""{var @__e=ON_MakeEnumerator($e);while ($v1=${messageS("@__e","nextValue")})!=${messageS("ON_Nil","make")} $b}"""
			}
		
		def try_catch_expr: Parser[Any] =
			("try" ~> expr) ~ ("catch" ~> name) ~ expr ^^ {
				case t ~ e ~ b => s"try $t catch $e $b"
			}

		def switch_expr: Parser[Any] =
			("switch" ~> expr) ~ ("{" ~> rep(
				(("case" ~ expr) <~ ":") ~ statement
					|
				("default" <~ ":") ~ statement
					|
				lineComment
			) <~ "}") ^^ {
				case e ~ List()         => s"switch $e {}"
				case e ~ (b: List[Any]) => {
					var out = s"switch $e {"
					b foreach {
						case "case" ~ c ~ r => out += s"$c=>$r" + "\n"
						case "default" ~ r  => out += s"default=>$r" + "\n"
						case _              => ""
					}
					out + "}"
				}
			}

		lazy val op10: PackratParser[Any] = (
			("this" ~ "->") ~> name ^^ ("this.@@get_vars()."+_) /* fix this eventually */
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
			variable
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
			not(expr) ~> "(" ~> expr <~ ")" ^^ ("("+_+")")
				|
			expr
		)

		//objn statements:
		def objn_interface: Parser[Any] =
			(
				("@interface" ~> name) ~ opt(":" ~> name) ~ rep(
					("+" | "-") ~ (
						name
							|||
						rep1((name <~ ":") ~ name ^^ {case l~r => (l, r)})
					)
						|
					"@property" ~ name ~ opt("(" ~> rep1sep(name, ",") <~ ")") ~ opt("=" ~> expr <~ ";")
						|
					lineComment
				) <~ "@end"
			) ^^ {
				case n ~ Some(i) ~ List() => s"var $n = @class.new(" + "\"" + n + "\", " + i + ")"
				case n ~ None ~ List()    => s"var $n = @class.new(" + "\"" + n + "\", null)"
				case n ~ (i: Option[Any]) ~ (body: List[Any])   => {
					var out = ""

					i match {
						case Some(i) => out += "var "+n+" = @class.new(\""+n+"\", "+i+")\n"
						case None    => out += "var "+n+" = @class.new(\""+n+"\", null)\n"
					}

					body foreach {
						case "@property" ~ p ~ Some(a: List[Any]) ~ None => out += s"$n.@@add_var("+"\""+p+"\", {" + (a map (""+_+"=>true") mkString ",") + "}, null)\n"
						case "@property" ~ p ~ None ~ None               => out += s"$n.@@add_var("+"\""+p+"\", null, null)\n"

						case "@property" ~ p ~ Some(a: List[Any]) ~ Some(v) => out += s"$n.@@add_var("+"\""+p+"\", {" + (a map (""+_+"=>true") mkString ",") + s"}, function(){$v})"+"\n"
						case "@property" ~ p ~ None ~ Some(v)               => out += s"$n.@@add_var("+"\""+p+"\", null, function(){"+v+"})\n"

						case "+" ~ (m: List[Any]) => out += s"$n.@@add_class_method(" + argsToSEL(m.asInstanceOf[List[(Any, Any)]]) + ")\n"

						case "+" ~ m              => out += s"$n.@@add_class_method("+"@SEL($array(\""+m+"\"),$array()))\n"

						case "-" ~ (m: List[Any]) => out += s"$n.@@add_instance_method(" + argsToSEL(m.asInstanceOf[List[(Any, Any)]]) + ")\n"

						case "-" ~ m              => out += s"$n.@@add_instance_method("+"@SEL($array(\""+m+"\"),$array()))\n"

						case _ => ""
					}

					out
				}
				case _ => ""
			}

		def objn_implementation: Parser[Any] =
			(
				("@implementation" ~> name) ~ opt("(" ~> rep1sep(name, ",") <~ ")") ~ rep(
					("+" | "-") ~ (
						name
							|||
						rep1((name <~ ":") ~ name ^^ {case l~r => (l, r)})
					) ~ block
						|
					lineComment
				) <~ "@end"
			) ^^ {
				case n ~ None ~ List()              => ""
				case n ~ None ~ (body: List[Any])   => {
					var out = s"$n.@@implementation();"

					body foreach {
						case "+" ~ (m: List[Any]) ~ b => {
							val mm = m.asInstanceOf[List[(Any, Any)]]

							out += s"$n.@@class_method(" + argsToSEL(mm) + ", function(args) {var " + (mm map {t=> ""+(t._2)+"=args."+(t._1)} mkString ",") + ";\n"+b + "\n})\n"
						}

						case "+" ~ m ~ b              => {
							out += s"$n.@@class_method("+"@SEL($array(\""+m+"\"),$array()), function() {\n"+b+"\n})\n"
						}

						case "-" ~ (m: List[Any]) ~ b => {
							val mm = m.asInstanceOf[List[(Any, Any)]]

							out += s"$n.@@instance_method(" + argsToSEL(mm) + ", function(args) {var " + (mm map {t=> ""+(t._2)+"=args."+(t._1)} mkString ",") + ";\n"+b + "\n})\n"
						}

						case "-" ~ m ~ b              => {
							out += s"$n.@@instance_method("+"@SEL($array(\""+m+"\"),$array()), function() {\n"+b+"\n})\n"
						}

						case _ => ""
					}

					out
				}
				case _ => ""
			}

		//statements:
		def var_decl: Parser[Any] =
			"var" ~> rep1sep(name ~ opt("=" ~> expr), ",") ^^ {l=>
				"var " + (l map {
					case n ~ Some(v) => s"$n = $v"
					case n ~ None    => s"$n"
				} mkString ",")
			}

		def return_stmt: Parser[Any] =
			"return" ~> expr ^^ ("return "+_)

		def break_stmt: Parser[Any] =
			"break" ~> opt(expr) ^^ {
				case Some(v) => s"break $v"
				case None    => "break;"
			}

		def continue_stmt: Parser[Any] =
			"continue"

		def func_stmt: Parser[Any] =
			(("function" ~> name) ~ ("(" ~> (repsep(name, ",") ||| (name <~ "...")) <~ ")") ~ block) ^^ {
				case n ~ List() ~ b         => s"$n = function()$b"
				case n ~ (a: List[Any]) ~ b => s"$n = function(" + (a mkString ",") + s")$b"
				case n ~ a ~ b              => s"$n = "+"$varargs(function(" + s"$a)$b)"
			}

		def import_macro_stmt: Parser[Any] =
			("#import" ~> ("\"(?:\\\"|[^\"])*?\"".r)) ^^ {
				case e => {
					val name = s"$e".init.tail

					if(name endsWith ".neko")
						("\n/* " + name + " */\n\n" + Source.fromFile(name).mkString + "\n")
					else if((name endsWith ".mn") || (name endsWith ".hn"))
						("\n/* " + name + " */\n\n" + parser(Source.fromFile(name).mkString) + "\n")
				}
			}

		def statement: Parser[Any] = (
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

		def lineComment: Parser[Any] = "//(?:[^\\n]*)".r ^^^ ("")
		def program: Parser[Any] = rep((statement ||| lineComment) <~ opt(";"))

		// parse thing
		def apply(input: String): Any = {
			parseAll(program, input) match {
				case Success(result, _) => (result.asInstanceOf[List[Any]]) mkString ";\n"
				case failure: NoSuccess => scala.sys.error(failure.toString)
			}
		}
	}

	val parser = new ONParser
	val flag = "^\\-.*".r
	
	args(0) match {
		case "-h" | "--help" => println("Usage: `objn-parser [options] filename` (you don't need the file extension).\n\n" +
			"-h, --help | display this message.\n" +
			"-c         | compile to Neko only.\n" +
			"-b         | combole to Neko Bytecode only.")
		
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
			var output = s"// ${args(0)}.mn" + "\n\n" + parser(sourceFile)
			
			new PrintWriter(args(0) + ".neko") {write(output); close}
			
			s"nekoc ${args(0)}.neko".!
			s"neko ${args(0)}.n".!
		}
	}
}
