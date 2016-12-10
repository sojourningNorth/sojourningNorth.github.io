---
title: What can you do in Armored Bits anyways?
author: Brian Jones
---

Two blogs posts in one day? Crazy. I suppose there's actually a bit to talk about now that a large chunk of AB's development is behind us.

This one is a brief overview of mech components and basic AI and server interactions.

## What is a mech composed of?

We plan on writing some detailed documentation about this at some point, but for now I'll provide a high level overview of what the components of a mech are and their basic interactions.

### Actuator

Kind of a loaded, misused term, it describes the actuator/servo/rotateable component typically found at joints such as the shoulders, or the waist.

### Arm

These typically have weapon mounts and can be armored.

### Armor

Covers the front and back locations of primary mech locations such as the cockpit, torso, legs, and arms.

### Capacitor

Certain weapons such as lasers require extra energy on demand to fire, so capacitors fill that role. They charge from the reactor.

### Cockpit

This contains the sensitive components of the mech, such as computers and sensors. While a small target, completely disabling it means a mech can no longer operate. It can be armored.

### Communication Device

This is a component which allows AI to send arbitrary data to each other, however, at a capped capacity, and different rates and number of channels depending on the device. There are currently radio (anyone can listen in), laser (line of sight with the target mech), and satellite (delayed but targeted) versions.

We're kind of excited to see how the community utilizes these.

### Computer

A computer allows a mech to lock on to a target, track a specific number of targets, and is required for certain weapons such as fly-by-wire missiles to operate. More powerful computers will contain more detailed information about the composition of enemy mechs, etc.

### Counter Measure

These range from anti missile systems, to signal jamming and EMP hardening.

### Engine

Without this a mech can't move. Different sizes and manufacturers offers different abilities, such as acceleration rate, top speed, turn radius speed, etc.

### Gyro

If this gets destroyed a mech can no longer move. Our physics system currently isn't sophisticated enough to handle falling over however.

### Leg

While nothing special occurs with legs, they provide armor, and are required for movement. As legs become damaged mech speed slows down.

### Reactor

The power plant which drives the entire mech. It provides a specific amount of power each game tick that actively powered components use to stay online. Creative players could possibly overload their mech with more components than the reactor could effectively power, but rotate them in/out as the battlefield demands change.

### Sensor

There are active and passive versions of sensors each with benefits and drawbacks. Without a sensor the mech cannot find and lock on to a target.

### Storage

This component allows the mech to store additional ammo. If it takes damage there is a chance the ammo can go critical and deal damage to the mech.

### Torso

This is the bit that ties the whole mech together. It houses the cockpit, engine, and reactor, as well as connects the arms, legs, and cockpit. It can be armored.

### Weapon

There are various weapon types, such as standard ballistic firing cannons, missile launchers, lasers, and mortars. Certain weapons require capacitors to be attached to them to fire.

### Projectile

Not a mech component, but contains the info related to the type of ammo a weapon fires, or what goes into a storage component. Certain projectiles such as guided missiles require an active computer in the mech to correctly track their target.

## Interaction

The player's AI communication with the server using Google [protocol buffers](https://developers.google.com/protocol-buffers/). We've broken these down into a few sets of messages.

1. Configure - These are used to configure the mech with desired components, and will error out if certain configurations are not possible. If the mech fails to be configured by the time the match starts the AI will be provided with a default pre-configured mech.
2. Query - The above components are sent back in an on demand hierarchical message which describes the state of each component, including things such as whether they have power, how much ammo is left in a weapon, data in a communication queue, etc.
3. Commit - These are small messages which the SDK can use to interact with the mech. For example, to set the mech engine's desired velocity, or to tell a weapon to enter the fire state.
4. Server - These are general messages from the server, such as a radio broadcast from a communication device, or whether the mech has gone into an out of bounds region.

There are also a separate set of messages which send simulation state to the visual client that the player uses to view a match in progress.

## Tying it Together

Hopefully with the above information one can begin to imagine how things start coming together.

Using the visual client you join the match making service. Once enough players are found they are thrown into a server divided into two teams. The player written AI code are also loaded into their own separate running server processes, and if they were written correctly will connect up to the game server which was spun up for this specific match. Once everyone's AI is connected (or the connection timeout has passed) the server enters the configuration phase. Once everyone's AI has configured their mechs (or the configuration timeout has passed) the server passes into the game phase. Once an enemy team is eliminated or a game timeout has passed the match is over.

While the game is actually running the player AI can query the server for their mech state at about 120 times per second (not concretely decided yet). Examining the state, the AI can then make decisions about how to turn, where to fire, what data is available about enemy mechs, what data is going in and out of communication devices (if it's say, radio), which targets they have a lock on, what individual armors levels are at, etc.

Above all a well written AI will be able to correlate all of this data and make good tactical decisions to destroy the enemy and not be destroyed itself. We envision sophisticated teams writing custom encoded communication protocols to coordinate their mechs, track down enemies, and destroy them in a coordinated effort.

## More to Come

The above describes everything that is currently in the game, however, there are still a handful of additional features we'd like to throw in before we make it to the official release. Hopefully as we begin to get the community engaged more with us we'll receive more suggestions for new and interesting features we could add.

If you are just coming across our blog, check out [http://armoredbits.com](http://armoredbits.com) and sign up to our newsletter at the bottom of the page for more updates as we get closer to release.
