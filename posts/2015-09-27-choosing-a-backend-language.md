---
title: Choosing a Backend Language
author: Brian Jones
---

Picking a language to write your game/system/service in shouldn’t really be a hard choice, yet as developers we tend to get hung up on which technology we want to use.  After all, you will be stuck with your choice once steam picks up, and for a myriad of reasons (usually business) switching stacks becomes incredibly hard the further along you are.

Since we are building a realtime game server (in the First Person Shooter sense), we had a small list of criteria.

1. Statically typed, non-dynamic.
2. If GC’ed, low overhead.
3. No runtime environment, compiled machine binary.
5. Good library support (TCP/UDP networking, linear algebra, messaging protocols).
6. Fast.

We were also inspired by John Carmack’s Quakecon 2013 speech about how he felt that using Functional Programming could reduce the number of errors even talented C/C++ programmers tend to create in game engines.  This really got us excited about looking at what the functional programming world had to offer.

While C/C++ are well known for being used to write incredibly low-level and fast code, especially when talking about game servers and game engines, we were hoping to avoid programming at such a low level in languages which are well known for this niche.  We’re building a game where you program to control robots inside it; we felt we couldn’t just take the first off the shelf language and immediately dive in, that would have been unsatisfying to us as programmers ourselves.

Still, picking a language is hard. It sounds incredibly stupid, especially as you gain more experience over the years, but when the world is your oyster you agitate over making the best choice you possibly can.

The following is a list of languages we explored, and what guided our decision to use them or not.  This isn’t a comprehensive breakdown of specific language features, it’s just a general overview of what we saw vs. what we wanted.

## C/C++

As stated above, while these are perfect choices for a game server, we wanted to try something different.

## Google’s Go Language

A compiled, garbage collected language, generally mocked by naysayers because of its lack of generics and disregard for modern day innovations in Computer Science language research.  The type system is admittedly quite weak, although better than nothing when compared to dynamic languages.

That said, it’s actually quite an enjoyable language to use when you just want to Get Things Done.  We’re intimately familiar with Go due to using it at our day job and on-the-side open source projects, so it wouldn’t really have been that bad of a choice if we wanted to try to prototype our game server as quickly as possible.

However, at the time of our initial exploration it was still at version 1.4.x which had some very high garbage collection pauses; not good for a high performance game server.  After 1.5 was released pauses in the 95th percentile were guaranteed to be no higher than 10ms, which made it extremely tempting to go back to and give everything a rewrite at that point.

## Rust

Probably one of the most exciting low-level system languages to be released as a direct competitor to C/C++ these days, it is a compiled GC-less language with compile-time safety guarantees.

Extremely tempting to use, we wrote a bit of test code with it to get a feel for the language.  If you can get over the conceptual hurdles of ownership and borrowing that Rust introduces, it is quite enjoyable to develop in.  However, the ecosystem is still relatively new, and we were faced with the possibility of having to write libraries which may have been missing or lacking.

One interesting thing to note is that we tinkered with Rust after playing with Haskell, and were quite happy to see how much influence the latter has played on the language design decisions.  There is a lot of power here.

## OCaml

I honestly can’t say much about this language because we never wrote a line of code in it.  It clearly has a very great engineer oriented community, and is a solid functional programming language aimed at systems programming.

It was definitely a close 2nd or 3rd to take a close look at, however the next entry explains the rest of the story.

## Haskell

For better or for worse this is ultimately the language we have chosen.  One could even argue it was a poor choice, as we had to spend the first several months wrapping our heads around how to use it in the first place.

Still, as time has gone on our confidence level and productivity in this language has increased.  One typically hears the phrase “if it compiles, it does what you wanted” in regards to Haskell, and for the most part this has been correct in our experience thus far.  Our code is extremely dense and concise, incredibly easy to refactor, and the library ecosystem has a lot to offer (I would argue no better or worse than Go’s given the maturity of various libraries in both languages).

When digging into topics about Haskell’s performance it’s interesting to note that because the language is immutable by default the GC can easily clean up garbage in parallel with execution.  There are also a lot of interesting optimization techniques, some of which show that Haskell can even reach C level execution speeds.

It’s not all without worry of course.  If you are not careful Haskell will generate something called thunks, which are chunks of code waiting for execution.  This is due to what is known as laziness, a primary feature of Haskell.  These can pile up and create another thing called space leaks, quickly running your machine out of memory, and can be incredibly hard to debug.

## Other

These are languages we had also touched in the past, but didn’t fit our criteria.

* Racket - An incredibly well documented Scheme (Lisp) language, Carmack even used it to teach his son how to write a game.  Fails the statically-typed criteria.
* Erlang/Elixir - Almost overkill for what we wanted, more focused on high availability telephone and web services than high performance game servers.  That said, Battlestar Galactica Online was actually an MMO written in Erlang.  Fails the statically-typed and runtime environment criteria.

While there is nothing inherently wrong with a language which requires a runtime, we don’t require any high level of portability.  As long as our language will execute on a Linux/FreeBSD machine in a simple binary format we are happy.

Static typing, however, is something we are pretty adamant about.  It is really quite troublesome catching errors at runtime, and no matter how many tests you write, full test coverage is nearly impossible.  We feel that compilers exist for many reasons, and one of those reasons is to help the developer find bugs they introduced long before the code comes even remotely close to a production system.

## Conclusion

In the end we picked Haskell.  It’s still a bumpy ride, when trying to dig around and find out how to do something we end up at blog posts with labels like Algebraic Data Types are a Category of Endfunctors in a Monad Burrito Okinawa Essa Festival, sometimes leaving us confused.  However, if we simply ignore the more research/mathematic oriented stuff, or ask nicely in the IRC #haskell channel, we’ve usually been able to figure out how to do what we want to do.

I’d say we’re about 35% done with the core server code base now, and the next 65% will probably just get easier to write because we’ve actually knocked out all the hard stuff already.  As we continue to push forward we’ll throw out a blog post or two explaining more of the exciting things we are trying to achieve by building a programmable battle simulator.
