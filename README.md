Mathematica Auto-completion Tweaker
================================

Changes the behavior of the auto-completion in Mathematica version 9 so that camel-humps completion can be used.
Camel-humps allow not only expansion of prefixes like `Integ` for `IntegerDigits`, they allow additionally to specify
the capital letters (the humps) in function-names only and therefore, e.g.  `ListLiPlo`, `ListLPlot` or even `LLP` can be 
expanded to `ListLinePlot`.

To be really fast with this kind of help you have to try avoiding arrow-keys for going down the suggestion list. 
Instead, you should specify your wanted function further and further by leaving out the small letters between capital 
camel-hump letters. A very good example is `Hypergeometric0F1`. Instead of typing `Hy` and going down the list very far, 
you leave out 12 letters and type `Hy0`. In many, many cases this leads to only one unique choice of an expansion and 
you only have to press <kbd>Enter</kbd> (or <kbd>Tab</kbd>) to accept it.

If there are still several choices in the end, *then* you can use your arrow keys. One example here is, when you 
typed (heroically) `LLLP` to shorten `ListLogLinearPlot` and see that there is a `ListLogLogPlot` with the same 
abbreviation. Unfortunately, at this point even typing `LLLPlot` does not help to make it clear and you have to use 
the arrow key to choose the right function.

**Completion of usual function names** works now both ways:

1. You get valid expansions if you type a *prefix* of the function like `Integ` for `IntegerDigits`.
2. Additionally, you can type `IntDig`, `InDi` or just `ID` to get `IntegerDigits` too. Important is that you take care of the capital letters in the names.

> ![enter image description here][http://i.stack.imgur.com/8gwTv.gif]

Support for **completion of `Options`** was added too.

> ![enter image description here][http://i.stack.imgur.com/Ly4aR.gif]

Additionally, camel-humps even work **after context specifications**. Note, that the context-names themselves cannot be camel-humps expanded because in most cases like `Developer`, `Experimental`, `Internal`, etc. this would be useless anyway.

> ![enter image description here][http://i.stack.imgur.com/DoJWG.gif]

Additionally, when I had a first version of this running, I created a teaser screen-cast

<iframe width="1280" height="720" src="http://www.youtube.com/embed/4JMkR2OeUuI" frameborder="0" allowfullscreen></iframe>

or with classical music here

<iframe width="1280" height="720" src="http://www.youtube.com/embed/iIG8_Pmzjn0" frameborder="0" allowfullscreen></iframe>

(viewer from Germany have to watch this [without sound](http://youtu.be/mzFfNbl9sr4)

Installation
------------

A simple `Get` of the `MathematicaAutocompletionTweaker.m` package turns the camel-humps completion on:

```
Get["http://goo.gl/wtVze"]
```

In Mathematica 9 you should now see a suggestion box when you type e.g. `PP`. If you want to turn it off again can 
restart the kernel or you use ``FE`RecoverDefaultCompletion[]``. If you want to install it permanently you can for 
instance copy the contents of the package to your `init.m`.

Related discussion on stackexchange
---

http://mathematica.stackexchange.com/a/16710/187