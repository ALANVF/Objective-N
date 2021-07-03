This is a short guide that shows various features of Objective-N.


# Neko features
Objective-N is 90% compatable with regular Neko code, so I'd recommend checking out their docs and specs on their website [here](https://nekovm.org/specs/syntax/) before continuing.
The other 5% of Objective-N that is *not* compatable with Neko includes these things:
- The `@` character is not allowed in identifiers.
- Curly braces are mandatory for anonymous function bodies.
- Multiline comments are not yet supported.
- Some statements like `return` can't be used with `&&` or `||`.


# Objective-N features

### Shorthand functions
Similarly to Objective-C, you may use `^` for anonymous functions rather than the `function` keyword. You may also omit the `(...)` if it doesn't take any arguments. They can also have parameter and return type annotations.

Here are some examples:
```js
function(a, b) {return a + b}
function {1 + 2}
^($tint a): $tint {a ** 2}
^{thing()}
^: $tstring {"hello"}
```

### Function declarations
Functions can be declared in a similar way to functions in JavaScript.
```js
function add(a, b) {
	return a + b
}
```

They can also have parameter and return type annotations
```js
function add($tint a, $tint b): $tint {
	return a + b
}
```

### Varargs syntax
For all kinds for regular functions, you may use `...` on the parameter of a function to indicate that it is variadic.
```js
function thing(values...) {...}
var thing = ^(things...) {...}
```

At some point, you'll be able to have multiple parameters while also having the function be variadic.


### More operators
Added operators:
- `**`: Exponentiation.
- `~`: Bitwise not.
- `!`: Logical not.

All operators except for `&&` and `||` are overloadable by Neko objects and Objective-N classes.

TODO: add more info later.

### For loops
Self-explanatory I think.
```js
for(var i = 0; i < 5; i++) {...}
```

### For-In loops
Basically iterates through a list of values. Works with Neko arrays, Neko objects (as far as I know), and any Objective-N type with an `ON_Enumerator` or `ON_Enumerator2` method.
```objective-c
for(var v in @[5, 10, 15])
	$print(v, "\n")

for(var i, v in @[5, 10, 15])
	$print(i, ",", v, "\n")

for(var k in @{@"a": 1, @"b": 2, @"c": 3})
	$print(k, "\n")

for(var k, v in @{@"a": 1, @"b": 2, @"c": 3})
	$print(k, ": ", v, "\n")
```

Might consider allowing iteration through hashtables at some point in the future.

### C-style switch statements
Mostly self-explanatory, but you can only have 1 statement after a case/default label for now, and break isn't mandatory.
```c
switch 2 {
	case 1: ...
	case 2: ...
	case 3: ...
	default: ...
}
```

Neko-style switches are still available.

### Directives
TODO.


# Objective-N object-oriented features

### Objective-N literals
Will probably go into more detail on this later, but here's the gist of it:
- `@123`: `ON_Integer` literal.
- `@12.34`: `ON_Float` literal.
- `@"abc"`: `ON_String` literal.
- `@[1, 2.3, @"c"]`: `ON_Array` literal.
- `@{@"a": 1, @"b": @[]}`: `ON_Dictionary` literal.
- `@(value)`: Boxing literal. Converts the Neko value `value` into an Objective-N value.
- `nil`: `ON_Nil` literal.
- `NULL`: `ON_Null` literal. No actual use other than to emulate Objective-C.

### Message calls
They work the same way that they work in Objective-C.
```objective-c
[a b]
[a b: c d: e]
[[a b] c: d]
```

### Classes
They work similarly to how they work in Objective-C, however there are some differences (each these will be explained in detail later):
- Properties work differently (and take less effort).
- Message declarations may not have datatypes until their implementation (for now).
- Message definitions may omit datatypes for various arguments.
- Messages do not have specified return types.
- `this` is used rather than `self` (might change eventually).
- Class members inside the class may only be accessed through `this->member` (for now).
- Class members outside the class may only be accessed through messages (for now).
- Protocols are not supported yet.
- Categories are not supported yet.
- Extensions are not supported yet.
- `make` is used rather than `alloc`.
- Messages sent to `nil` will fail.
- `instancetype` is not supported yet.

Here's a basic example of a class:
```objective-c
@interface Basic: ON_Object
    @property ivar1 (read, write)
    
    - init
    - initWithValue:value
@end
        
@implementation Basic
    - init {
        $print("created new Basic\n")
        this->ivar1 = 1
        return this
    }
    
    - initWithValue:value {
        $print("created new Basic with ", value, "\n")
        this->ivar1 = value
        return this
    }
@end

var b1 = [[Basic make] init],
    b2 = [[Basic make] initWithValue: 5]

$print([b1 ivar1], "\n", [b2 ivar1], "\n")
```

### Optional typing
TODO: add more info here.
```objective-c
- method1:value                     // can be any type
- method2:($tint)value              // can only be a Neko integer
- method3:($tint | ON_Integer)value // can either be a Neko integer or an instance of ON_Integer
- ($tint) method4                   // returns a Neko integer
var ints: $array($tint) = $amake(5) // ints is an empty array of 5 Neko integers
```