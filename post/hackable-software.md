## Philosophy

Hackable software is an approach to write, maintain and distribute minimal programs for personal use.

> Software is:
>
>   - doing not more and not less than what the user needs
>   - made for users capable of reading and customizing its source code
>   - distributed as source code, compiled by the user
>   - not meant to be developed forever and its final state should be described by its feature set from
> the beginning
>   - extended either by the user directly, or by applying
> [source code patches](https://en.wikipedia.org/wiki/Patch_(computing)#Source_code_patching) distributed as diff files

Now... let's start from the beginning.


## What is hacking

> [hacker: n.](http://www.catb.org/jargon/html/H/hacker.html)
>
> [originally, someone who makes furniture with an axe]

But today we will not be talking about carpenters, [MIT hackers](https://en.wikipedia.org/wiki/Hacks_at_the_Massachusetts_Institute_of_Technology),
or [security researchers](https://en.wikipedia.org/wiki/Security_hacker). But rather:

> [hacker: n.](http://www.catb.org/jargon/html/H/hacker.html)
>
> 1.\ A person who enjoys exploring the details of programmable systems and how to stretch their capabilities,
> as opposed to most users, who prefer to learn only the minimum necessary.
> RFC1392, the Internet Users' Glossary, usefully amplifies this as:
> A person who delights in having an intimate understanding of the internal workings of a system, computers
> and computer networks in particular.
>
> 2.\ One who programs enthusiastically (even obsessively) or who enjoys programming rather than just theorizing about programming. 

## UNIX vs Enterprise

One can seemingly categorize all software in the world into programs that can do everything and programs that do only
one thing.

> Every program attempts to expand until it can read mail.
Those programs which cannot so expand are replaced by ones which can.
<p style="text-align: right">-\ [Zawinski's Law](https://en.wikipedia.org/wiki/Jamie_Zawinski#Zawinski's_Law)</p>

This is a natural process in a commercial setting where more features mean more customers and more money.
But this is not the _only_ way.

There are programs that were designed to do one thing and do it well. You might know some of them, and most likely because
they were originally a part of a [UNIX operating system](https://en.wikipedia.org/wiki/Unix) in the '70s.
I'm talking about command line utilities such as `ls`, `cat`, `sudo`, etc.

![[XKCD-149](https://xkcd.com/149/)](https://imgs.xkcd.com/comics/sandwich.png)

The idea behind these minimal, modular programs is now known as [Unix philosophy](http://www.catb.org/esr/writings/taoup/html/ch01s06.html):

> - Write programs that do one thing and do it well.
> - Write programs to work together.
> - Write programs to handle text streams, because that is a universal interface.
<p style="text-align: right">-\ [Peter H. Salus in A Quarter-Century of Unix (1994)](http://www.catb.org/~esr/writings/taoup/html/)</p>

## Software cafeteria

In context of hackable software, it's important to notice that likely all software you use can do more than _you_ need from it.
Commercial software is designed as food in a college cafeteria - nobody particularly likes it, but it's the best
common denominator.

And minimalist Unix software won't help there much, because on their own such tools are not very powerful - power
emerges when they are combined together (output of one is input of another) to solve non-standard task.
In this analogy simple programs are ingredients and in hands of a great cook they become a tasty meal.

![[Russian school kitchen, pots (likely) with black tea](https://chel.aif.ru/society/sekretnyy_ingredient_za_chto_deti_i_roditeli_lyubyat_shkolnuyu_stolovuyu)](https://aif-s3.aif.ru/images/031/310/f707b00737261d6bf7fcb3c8be2c016a.jpg)

How would a software from a fine dining restaurant look like?
Building software for personal use of a single wealthy client is not a real thing, so it seems like we have to rely on
big companies with lots of money and smart engineers to make good programs that would fit every user at once.

Wait, **home cooking**! It's a big thing, but making custom programs from scratch is hard.
The amount of foundational knowledge needed to write and _understand_ the first ever line of code is monumental.
More approachable way would be to rely on simple programs to do the heavy lifting and combine them to produce something meaningful.
Brian Kernighan [mentioned](https://youtu.be/WEb_YL1K1Qg?si=WEk4i1c6Ju8lRP0L&t=1306)
it in a recent talk about the history of UNIX, how powerful combination of a simple programs can be in the right hands.
As an example, [this](https://unix.stackexchange.com/a/30852/368590) command can be used to forcefully stop application `conky`:

```sh
ps aux | grep conky | grep -v grep | awk '{print $2}' | xargs kill
```

This is a great starting point to gradually explore the world of programming, along with [visual scripting](https://en.wikipedia.org/wiki/Visual_programming_language)
and [problem solving games](https://www.zachtronics.com/).

"I'm ready to write some code!" - you said.
Good, the rest of this post is for amateur cooks and professional chefs who would like to make meals for themselves.

## Suckless

The idea of tailoring programs to your own needs is not new.
Most notable examples include programs released in 2006 by [Suckless community](https://suckless.org/):
X11 window manager [dwm](https://dwm.suckless.org/), terminal emulator [st](https://st.suckless.org/),
menu utility [dmenu](https://tools.suckless.org/dmenu) and [others](https://suckless.org/other_projects/).

What distinguishes suckless software from other personal use programs developed today is that configuration is done
directly in the source code. This also means that traditional way of distributing software in binaries doesn't work -
full source code and build instructions are needed.

## So what is hackable software

Hackable software is the idea of writing programs for hackers. Let's expand on the principles a bit.

### Software is doing not more and not less than what the user needs

Every feature is implemented in the most simple and straightforward form: keep it simple, fast, and clear.
It's easier to protect simplicity when additional functionality is out of scope.

### Software is made for users capable of reading and customizing its source code

It's great when source code can be read and understood by the user in one evening.
User manual documentation is not necessary, since concise code is better than plain text documentation.
But _hacking_ documentation is encouraged (via `HACKING.md`), providing help on building the program,
high-level design overview, and other notes that would help grasping the code.

### Software is not meant to be developed forever and its final state should be described by its feature set from the beginning

In many cases, code quality is compromised because of a wrong assumptions and dynamically changing requirements.
Think about designing a bicycle not knowing it would be needed to make it a motorcycle along the way.

### Software is distributed as source code, compiled by the user

Because users need to configure programs by modifying the source code, they need to have it in full.
Also, please provide build instructions in case it's not obvious from the build system config (`npm run build` or
`cargo build` or `zig build` or `make`)

### Software is extended either by the user directly, or by applying source code patches distributed as diff files

Extending functionality is often hard and requires architecture changes, so it might be a good idea to not break
it for all user, even those who don't need it.
So it makes sense to keep additional functionality optional by providing patches.
It is also encouraged to share your extensions with others who can find it useful.

## Evaluation

Why would anyone use it, who is it for, and what are drawbacks.

### The good

For developer:

  - Focus on core functionality. Don't worry about very specific edge cases. Meaningful panic message is good enough
for a user to fix it in case it happens to be relevant.
  - Lots of complexity can be dropped. Forget about configuration file hell - user preferences are "hardcoded".
No need for packaging - responsibility is shifted to the user.
Providing clear build instructions is a good idea though.
  - Distribution is as simple as it gets. Link to the source code or a tarball is all that's needed.
  - Incentive to keep code concise and simple - many people gonna mess with it.
  - Users are capable. More detailed bug reports, productive discussions, high-quality contributions.
  - Less code means less exposure to vulnerabilities and bugs, less maintenance.

For user:

  - No [bloat](https://en.wikipedia.org/wiki/Software_bloat). Program you get is simple and straightforward.
  - Adding new functionality is as simple as it can be. Making changes to a 1k LOC codebase is a thousand times simpler
than to a 1M LOC one.
  - Native compilation. Since you compile it on your machine, compiler gets best hardware optimizations.

### The bad

Like any approach, hackable software has its limits and drawbacks:

  - Does not work in enterprise setting. Software is not furniture to sell disassembled. This leaves hackable software
for **hackers**: open-source enjoyers, programming hobbyists, and students.
  - Compile it yourself.
You need to satisfy build requirements, such as having the right compiler and system dependencies.
Not as bad as it sounds, because keeping software simple also means keeping build process simple.
It's possible to build software [without](https://yzena.com/2024/03/build-system-schism-the-curse-of-meta-build-systems/)
meta build systems.
It might be a good idea to use a programming language directly to describe a build process ([build.zig](https://ziglang.org/learn/build-system/)
or [nob.h](https://github.com/tsoding/nob.h)).
  - Only programmers could use your program. Hackable software is a bad approach to make commercial software, won't
work for a game you would want to sell on Steam.
  - Works well for executable programs, for libraries - not so much. Libraries need to have stable API to be generally
useful and this is not feasible. Solution is to submodule a library into the project so that its source is controlled
(not that uncommon in C/C++ land anyway).
  - Only computer users could use your program. Doesn't seem feasible to modify and recompile programs on smartphones,
gaming consoles, vacuum cleaners...
  - Not every piece of software can be small and concise. My favorite example is W3C browser specification, which is
[114 million words long](https://drewdevault.com/2020/03/18/Reckless-limitless-scope.html).
Same would be true to other spec-dependent software: OS components, hardware drivers, embedded firmware.
Does not imply that we should not try to keep is simple and not fight back against incidental complexity.
    ```text
                         Software hackability

    how difficult it
    is to write
    from scratch
          │                             Web browser
          │                         
          │                                      OS
          │
          │
          │                           Text editor
          │
          │                       Terminal emulator
          │                            App launcher
          │          ┌────────────────────────────┐
          │          │     simple automation      │
          │          │          scripts           │
          │          └────────────────────────────┘
          └──────────────────────────────────────── how often it is needed
                                                    and how much
                                                    customization is desired
    ```
  - Common denominator does not exist. There might be programs for which picking the core functionality is really
tricky, because every user's workflow is [very different](https://xkcd.com/1172/). This might mean that such program
should not exist... Anyway, use common sense when deciding what is considered core functionality and what not.
  - Maintaining patches is not much different from maintaining a program with all patches applied all at once.
Facilitating factor is that it's sometimes okay for patches to be broken (not applying cleanly), since resolving
conflicts is much easier than writing featrue from scratch.
  - It still might not be for you. Messing with source code, building, resolving patch conflicts, and debugging
stuff might be daunting. Seek other things in life that bring enjoyment, and taking a break is important.

## Plug

This post is a formulation of design ideas and principles I followed developing [hat](https://github.com/ivanjermakov/hat)
- modal text editor for modern terminals.

| ![Screenshot select](https://raw.githubusercontent.com/ivanjermakov/hat/refs/heads/master/img/screenshot-select.png) | ![Screenshot select](https://raw.githubusercontent.com/ivanjermakov/hat/refs/heads/master/img/screenshot-find.png) |
|-----------------------------|-----------------------------|
| ![Screenshot completion](https://raw.githubusercontent.com/ivanjermakov/hat/refs/heads/master/img/screenshot-cmp.png) | ![Screenshot diagnostics](https://raw.githubusercontent.com/ivanjermakov/hat/refs/heads/master/img/screenshot-diagnostics.png) |

I'm an active [neovim](https://github.com/neovim/neovim) user, and I found that [developing](https://github.com/ivanjermakov/cmdzero.nvim)
[plugins](https://github.com/ivanjermakov/troublesum.nvim) is harder than it needs to be.
100 LOC nvim plugin could be two-line patch in hat.
And after reading a bunch of neovim's source code I can confidently say that it is not as simple at it looks (depending
on how you read it, it might be a compliment to its UI).

## Rant on abstraction

And developing neovim plugins or tweaking the config is problematic because it's _abstract_. You don't need fundamental
knowledge to do that, but specific _neovim knowledge_. And more abstract technology becomes, the harder it gets to gain
such knowledge.

We can go a level higher and talk about beginner neovim users who decide to start their journey with [neovim distributions](https://github.com/topics/neovim-configuration)
or even [distribution managers](https://lazyman.dev).
By choosing a distribution, user has to learn how to solve problems in context of that specific distribution.
So much that core neovim knowledge might not suffice.
Not sure if there is a term for this effect, but it's often overlooked that abstraction does not make things simpler,
rather hides complexity under the rug.

My advice to picking the right abstraction level is to go as low as it is reasonable for your problem/knowledge and be
cautious of being too abstract. Abstraction is good until you need specifics of what it is hiding, then it falls apart.

## Further reading

  - [Hacks at MIT](https://en.wikipedia.org/wiki/Hacks_at_the_Massachusetts_Institute_of_Technology)
  - [Suckless Software for Everyone: You too can LARP as a good programer! (SELF 2023) (talk)](https://www.youtube.com/watch?v=slIxE8oYzus)
  - [The beauty of Unix pipelines](https://prithu.dev/posts/unix-pipeline/)
  - [Suckless software](https://suckless.org/)
  - [hat - modal text editor for modern terminals](https://github.com/ivanjermakov/hat)
  - [The Configuration Complexity Clock](https://mikehadlow.blogspot.com/2012/05/configuration-complexity-clock.html?m=1)
  - [Malleable software - Restoring user agency in a world of locked-down apps](https://www.inkandswitch.com/essay/malleable-software/#existing-approaches)

