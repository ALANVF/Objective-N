# Objective-N

## What is it?

Objective-N is an object-oriented language targeting the [Neko VM](https://nekovm.org/). You could think of it as a scriptable variant of Objective-C mixed with [Neko](https://nekovm.org/specs/syntax/).


## Running

Steps for running code are coming soon.


## Other things

- Obj-N's standard library is purposely modeled after Obj-C's standard library, with the exception of (temporarily) using the prefix `ON_` rather than `NS`.
- The parser/compiler uses Scala's default parser combinator library, which makes it really easy to add new grammar to the parser.
- Obj-N is more of a toy language than a useful/productive language, so it won't always be getting frequent updates.


## Possible questions

#### Q: Why don't you have any examples?
A: It's on my todo-list. For now, you could probably look at the standard library or test files. On that note, tests/mal is actually more of an example than a test, and it's based on the [MAL tutorial](https://github.com/kanaka/mal/).

#### Q: Can I use Obj-N classes in Haxe?
A: I haven't tested it, but you probably can.

#### Q: Can I use Haxe classes/functions/etc in Obj-N by using the Neko backend?
A: I haven't tested it, but you probably can since it uses modules. For identifiers that include `@`-signs, you can just use `$objget(value, $hash("my@property"))`.

#### Q: So I looked at the code for the parser...
A: Yes, I'm fully aware that the code looks ugly/messy.

#### Q: Why did it take nearly a year for you to update the readme?
A: Never got around to doing it.