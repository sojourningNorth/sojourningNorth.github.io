---
title: Doing Indie Game Dev Wrong
author: Brian Jones
---

Ok, admittedly the title is a bit misleading, but hear me out. We absolutely love what we are doing and are having a blast. However, if you truly want to be a successful indie game dev you probably shouldn't do 99% of the things that we are doing.

The goal of this post is to describe the challenges we face while making an incredibly non-traditional game, as well as give a feel for all the parts and pieces which are involved with our vision of the still far off release of [Armored Bits](https://armoredbits.com).

## Armored Bits Isn't Even a Traditional Game?!

That's right. If the game doesn't have player control can you really call it a game? If your target audience is programmers (or wannabe programmers) is it truly a game? If you spend more time coding to make your mech work than actually watch your mech fight is it really a game? There's a couple great examples of games-for-programmers out there by some talented game devs, but usually the programming mechanics only work within the context of the game itself as opposed to being an independent system.

## Picking a New Language is a Terrible Idea

Awhile back we discussed [choosing a backend language](/posts/2015-09-27-choosing-a-backend-language.html). I have fallen in love with Haskell. I absolutely love what you can do with the language, warts and all. I will never pick the language with the highest learning curve in the world to start a new project ever again. On the other hand, maybe now that I've found The Perfect Language™ I'll never have to worry about that particular case.

We could have had a working prototype written in some other language we were more familiar with within months, however, in hindsight I don't really regret the decision to go with Haskell at all. For most of the reasons people typically bring up it's been a pleasure to use. Incredibly easy refactoring, strong type system, very high level abstractions which result in less lines of code, easy to reason about, performant for soft realtime systems, etc.

I've also added a nice little bullet point to my resume, although not one that would ever help me get a game job. Continuing the trend of going against the grain I suppose.

## How Many Components Does it Have Again?!

Let me list off all the things Chris and I typically screw with each month.

1. Core Haskell game server.
2. Javascript game SDK.
3. Unity3d visual client.
4. Elixir.
    1. User management service.
    2. Code management service.
    3. Login management service.
    4. Match making service.
    5. Game server + user code "slug" orchestration service.
5. Server infrastructure (web, services, db, game, monitoring, etc.)
6. Project management (bitbucket + JIRA).
7. Promotional websites.
8. Social media.

A typical indie dev game usually doesn't have points 1, 2, 4, or 5 at all! What the hell were we thinking?

Still, since our primary focus is on programmable gaming we can get away with using placeholder art for the most part. The drawback of course is that we don't really have any bling to show off to our audience.

## Do You Have Day Jobs?

Not just day jobs, but I personally have a little nearly 2 year old daughter who is a full time job herself. AB dev time happens in the wee hours of the night or when time permits on weekends. This admittedly does not fall under the "you are doing it wrong" category, but it does impact the speed at which we can bring this project to release.

What is truly scary to ponder is whether all of this ends up being a waste of time or not. I truly hope with all my heart that we have found a niche which appeals to a decent number of people.

## For the Sake of Completeness

I suppose a post about a game wouldn't be complete without *some* kind of screenshot or video, so here is some (scripted!) footage of our mech assets in action for a promotion we plan on doing soon. Hopefully the actual player AI on player AI action looks at least this good.

<iframe width="1024" height="600" src="https://www.youtube.com/embed/wUnrhhkKIjc" frameborder="0" allowfullscreen></iframe>

Keep your eyes out for more information as we continue to move forward. I'm just a few more nights of hard work away from applying our alpha1 (internal test) tag to the core game server master branch, so we're getting there!
