---
title: Armored Bits - War Machines
author: Brian Jones
---

## Game AI Programming Simulator

In _A.I. Apocalypse_, book 2 of the Singularity series by William Hertling, the following phrases briefly describe a game played inside the novel:

> “Two years later the Mech War gaming platform was introduced just prior to the Christmas season. It became the must-have game. The old standby worlds went vacant, their online environments quickly becoming ghost towns. Mech War became not just the best massively multiplayer online game, it quickly became the only game left standing.”

> “Where other games had elaborate anti-cheat mechanisms to prevent people from using aimbots, Mech War provided plugin APIs for games to develop aiming algorithms. Where other games had server side monitoring to ensure gamers didn’t flit about the environment, Mech war provided a realistic physics model of the universe and a moving-parts-level simulation of in-game equipment.”

When I read these sentences a few years back it reminded me of a time well over a decade ago when I played a little online browser based game called Droid Arena. You wrote assembler-like code to control the behaviour of a little AI bot to play keep-away with a ball in a 2d grid maze vs other players. At the end of a round there was text output which basically told you the actions your bot took during the game, but that alone was enough to drive my interest for quite a long time.

As I age I notice that, as much as it dismays me to say this, my reactions are getting slower, and it’s harder to spend the necessary time to keep a competitive edge in competitive games while also trying to juggle a full time day job and a family. What I have truly wanted for years now is some kind of game that appeals on an intellectual level; one that can be played by people of all ages, at their own leisure, where the best mind wins.

Today, when I think about how to wisely use up what little spare time I have to focus on indie game development, I reflect back on William Hertling’s book and the game it described. Over the years I have tinkered with writing networked multiplayer game servers, and at the beginning of this year it became apparent to me what Uncanny Works needs to do to make this War Machine simulator a reality.

The rest of this blog post attempts to explain the design goals, the technology, and what I think it will take to push forward and release such a potentially amazing competitive learning game.

## The Game

The goal doesn’t stray too far from what was described by William Hertling, where players program AI to interact with an incredibly complex mech-like game simulation. Rather than create a large persistent world, however, our game will involve limited team matches via a match making system.

The ultimate goal of the player is to rise through the ranks of the league system, common amongst games today, with names such as Bronze, Silver, Gold, Platinum to denote how far up the ladder they are. Players can choose to participate in the ranked games or not, and may choose to join as a solo entrant with other solo players or join part of an existing team.

Players will have a game client with which they can use to interact with the system as a whole, and above all be able to watch their AI battle in its realtime 3d glory.

The majority of time, however, will likely be spent in a code editor as the player attempts to utilize modern day programming techniques to coordinate with their team and destroy their enemies.  From this could evolve novel communication techniques, cryptography, battle coordination amongst AI, optimal usage of onboard resources, the list goes on and on.  This part where players from around the world introduce ideas no one has ever thought of genuinely excites me.

## Business Model

At the end of the day the goal is to generate enough revenue so that the members of Uncanny Works can throw themselves into this project 100%. Our passion translates into a better experience for the players, whether it be more in depth game playability and more supported languages, or better documentation and tutorials, community events, and possibly even global tournaments.

Our #1 goal when we’ve discussed how to generate revenue is to never introduce any kind of element that requires a player to pay-to-win. Therefore, any monetary transactions which take place will strictly be cosmetic in nature. Whether it’s simply a skin for a War Machine giving it a distinct flavor and style, or a fully togglable environment skin which allows the player to view the entire battle in a grim dust-and-metal war theme, or something silly like War Machines dressed as clowns shooting confetti at each other.

Our second revenue model, and probably the most interesting, borrows from a recently revealed programming CTF game due for release later this year. Fundamentally the goal would be to allow us to contact top ranked players at the end of each game season whether they would be interested in talking to companies who wish to interview them. We simply become an engineering recruitment agency, except vastly cooler and way more credible because players are quite literally using their programming skills inside our game infrastructure to reach the top ranks.

What better way to display that you have incredibly advanced programming skills than to show that you are a top ranked player in a game which is driven by AI written by yourself? This eliminates the tedium of the written resume, and should a player wish to disclose their source and/or methods solid proof of their skill.

## Primary Development Goals

The following are some guiding principles I have been following while implementing the game:

1. The core server should be written in a language which embodies the concepts of a strong programming language, and above all be capable of running production level game simulation code.
2. Players must be able to use whichever programming language they desire to interact with the simulation.
3. New players/programmers should be guided via well written documentation and tutorials to get their feet wet and keep their interest engaged.
4. It must be visceral. This means a 3d client which allows games to be watched in their realtime glory.

## 1 - The Core Server

For the last 6+ months I’ve been learning Haskell when I have the time. The fact that functional programming is so different from imperative programming alone has made this an uphill battle for me, although I have finally reached a point where I am writing productive and elegant code.

While there are some technical tradeoffs when considering using this language, I felt that it may help make a solid statement about what Uncanny Works is trying to do here. Why not use one of the grand daddy of all research languages and make that the core of this game ecosystem? The benefits are already being seen as incredibly dense yet easy to read, reason about, and refactor, code. The drawbacks are admittedly there, such as laziness affecting the ability to reason about deterministic memory usage, space leaks and how to deal with them, etc.

That said, there is a functional server core in place. TCP communication with clients via protobufs, a game scene with updatable entities being processed, the complex War Machine component framework, and a bare bones API for interacting with War Machines from external player code.

## 2 - Programming Languages

One of the most important goals for us is to allow players to use whichever programming language they wish.  Should a language currently be unsupported, we would like to offer the tools required to help build the API SDK and ability to extend the platforms which allow it to be used within our stack.  We see no point in only targeting specific languages, and would like to aim for as diverse of an ecosystem as possible.

## 3 - Documentation and Tutorials

Without great documentation and tutorials it would be impossible for players to dive in and get started.  This is an absolute priority before a release ready game is introduced to the masses.

## 4 - Visceral 3d Client

While the majority of the player’s time will be spent programming, watching the fruits of their labor should be an incredible experience.

Similar to games like Gratuitous Space Battles where you set things up, push play, and simply sit back and watch incredible battles take place on your screen, we’d like the player to feel the tenseness and abruptness of the chaos of battle unfold before their eyes. 

For those that have played Mechwarrior Online, the experience of a hulking battle machine walking across the battlefield half disabled yet still a vicious monster should be prominent.  The armor shattering and screen shaking visceral-ness of weathering hits should have a player on their toes as they cheer their AI on.

## Conclusion

The path is extremely long, but we feel that given enough passion, time, and effort we can eventually get a product into the hands of players.  As we make more progress this blog will continue to get updated.

Now onwards towards an alpha build.
