## Why are you using camelCase instead of lisp-hyphens or snake_case?

Hoon and Lisp use-hyphens-to-combine-words-into-identifiers, and I have
always thought that this was the best of the approaches.

The most obvious downside is that it consumes the `-` rune, which
especially sucks for algebraic expressions `(x*y + y-z)`.

A second downside is that using `-` for identifiers, without hardcoding
that into Rex (which would prevent any Rex language from having a
`minus` operator) de-facto requires operator precidence rules:

    * x,list-len,[] (compute it)

But even with those downsides, earlier iterations of Sire still used
the hyphen-convention.

Unfortunately, because of the heavy use of operators (runes) in the syntax,
the hyphenated names result in visually noisy code.  `snake_case` is a
little bit better but not much.  and `camelCase` is far less noisy.

So that is why this convention was choosen.

And this is not much of a problem in Hoon, because the convention is
to use single-word names wherever possible.  If you take some piece
of complex Hoon code, and rewrite most of identifiers to have longer
multi-word names, then it starts to get very noisy as well.  Hoon suffers
from this a little bit less because it's runes are all two characters,
and open-runes are always spaced with two-spaces.

But Hoon is still an operator-heavy language, and expressions like
`bob+ted.foo-bar.sal` still suffer from this.  In contrast
`bob+ted.fooBar.sal` is MUCH clearer.
