# Best Practices in Cuis Smalltalk

This document is meant to shine some light on certain practices within Cuis that we recommend following for various reasons. It tries to help readers understand some less obvious design decisions without linking to the cuis-dev mailing list.

# Message Conventions

This section deals with message sending, the format of certain messages, and what message is supposed to be used when.

## Replace `#respondsTo:` with `#is:`

The message `#respondsTo:` which is used in code like

```smalltalk
  | foo |
  foo := Object new.
  (foo respondsTo: #saveAsModel)
    ifTrue: [ foo saveAsmodel ].
```

should be replaced by `#is:` which is derived from messages like `#isBag`, `#isDictionary`, `#isForm`, etc. A changed example would then look like this:

```smalltalk
  | foo |
  foo := Object new.
  (foo is: #saveableAsModel)
    ifTrue: [ foo saveAsmodel ].
```

The reasoning for this is:

1. `#is:` looks like the forms `#isBag`, `#isDictionary`, etc.
   which test for the type of the receiving object. 
   
2. `#respondsTo:` and the `#isBag` are of the same form,
   they both receive a symbol and return true or false if
   the object conforms in some way to the question asked
   in the argument. So they can be unified into `#is:`
   from that perspective.
   
3. `#isBag`, etc. require an implementation returning false
   high enough in the class hierarchy (for example, at Collection or Object).
   These general, abstract classes should be unaware of the specific knowledge
   of those down in the hierarchy. By default, `#is:` answers false to any unknown
   parameter value.
   
4. `#respondsTo:` goes up the class hierarchy when doing lookups,
   it looks into the selectors of the object, then goes up the class
   hierarchy until a result is found. At each level, an expensive dictionary lookup is needed.
   By using `#is:` the check can be faster, because checks are for identity of symbols.
   
5. `#is:` is more flexible. You can use it to ask arbitrary questions
   to the object. `obj is: Class. obj is: #Bag. obj is: 1.` and each
   object can decide how it handles this based on expectation.
   `#respondsTo:` is reserved more for testing if an object can handle
   a specific message.
