> In this post, _simple software_ means its interface and implementation are straightforward.
>
> I'm talking here about actual simplicity, not perceived simplicity.
> Therefore, software which interface is simple but hiding complex implementation is not considered simple.

## Early days (1950s)

<quote>when hardware is not fast enough for complex software</quote>

Early software had no room for incidental complexity because of how cumbersome it was to write.
Code was entered into computers in different cumbersome way: by flicking switches, inserting
paper [punched cards](https://en.wikipedia.org/wiki/Punched_card), or providing binary sequence
in some other way. Every bit or hole had to be correct, because in case programmer (or rather computer operator)
made a mistake, program would need to be loaded again.

This process was so unbearable that among the first programs were tools for programmers: assemblers and compilers.
Many of the well known programming languages appeared in that era: [Fortran](https://en.wikipedia.org/wiki/Fortran),
[Lisp](https://en.wikipedia.org/wiki/Lisp_(programming_language)),
[C](https://en.wikipedia.org/wiki/C_(programming_language)),
[Pascal](https://en.wikipedia.org/wiki/Pascal_(programming_language)), and others.

Although programming environment allowed some space for complexity - hardware certainly didn't,
with common hardware specs of 8 bit CPU, 15kB RAM, and 100kB flopply disk.

## Early operating systems (1960s-1970s)

In 1960s, majority of software was written for [mainframe computers](https://en.wikipedia.org/wiki/Mainframe_computer),
minority being various [military](https://en.wikipedia.org/wiki/D-17B),
[military](https://en.wikipedia.org/wiki/Saab_37_Viggen), and
[spacecraft (also military)](https://en.wikipedia.org/wiki/Apollo_Guidance_Computer) projects.
While portable computer hardware still had very limited capabilities, mainframe computers had big software projects
even by today's standards - initial 1966 release of a mainframe operating system
[OS/360](https://en.wikipedia.org/wiki/OS/360_and_successors) was written in about a 1M lines of code (LOC) of assembly
and [reportedly](https://public.dhe.ibm.com/s390/zos/racf/pdf/PPLD_History_of_the_System360_2024_04_24.pdf) grew to
10M LOC.

In 1969, Bell Labs (notaby Ken Thompson, Dennis Ritchie, Brian Kernighan, and others) released
[UNIX](https://people.eecs.berkeley.edu/~brewer/cs262/unix.pdf) - multi-user time-sharing OS.
It was initially written in assembly for [minicomputers](https://en.wikipedia.org/wiki/Minicomputer)
PDP-7 and (soon after release) for PDP-11, but later in 1973 UNIX was
[rewritten](https://www.cs.dartmouth.edu/~doug/reader.pdf) in about 100K LOC of newborn home-grown
[C](https://en.wikipedia.org/wiki/C_(programming_language)) language.

Many consider Ken Thompson the father of software simplicity for defining
[Unix philosophy](https://en.wikipedia.org/wiki/Unix_philosophy). It can be summarized in three points:

  - Write programs that do one thing and do it well.
  - Write programs to work together.
  - Write programs to handle text streams, because that is a universal interface.

Even though it was only [mentioned](https://people.eecs.berkeley.edu/~brewer/cs262/unix.pdf) in 1974 and
[published](http://www.catb.org/~esr/writings/taoup/html/ch01s06.html) in 1978, it was at the core of UNIX design from
the start.
Today, most (except Windows, more on that later) OSes take Unix philosophy seriously, which makes writing and
distributing cross-platform programs simpler.

## Personal computers (1980s)

In early 1980s, computer hardware became more affordable thanks to the mass production of microprocessors and other
off-the-shelf components, making computers available for personal use. This created a demand for software for users,
not scientists. Despite great adoption ([~100M](https://stats.areppim.com/stats/stats_pcxfcst.htm) of personal
computers by 1990), home computer use cases were blurry and
[not well understood](https://www.youtube.com/watch?v=2kut_LCtiNU) by regular people and not very accessible because
of console interface prevalence.

<!-- TODO: OOP mania -->

## Dot com (1990s)

## Mobile (2000s)

## LLM (2020s)

## Further reading

  - [Minuteman D-17b: The Desktop Computer Was Born in an ICBM [video]](https://www.youtube.com/watch?v=MJPnZzZtswc)
  - [The First Digital Flight Computer That Was Actually Any Good: The SAAB Viggen's CK37 [video]](https://www.youtube.com/watch?v=zf6bZBV7EWo)

## Ref

  - https://en.wikipedia.org/wiki/Wirth%27s_law
  - https://en.wikipedia.org/wiki/Worse_is_better
  - https://web.stanford.edu/class/archive/cs/cs240/cs240.1236/old//sp2014/readings/worse-is-better.html
  - https://en.wikipedia.org/wiki/Object-oriented_programming
  - https://en.wikipedia.org/wiki/Jamie_Zawinski#Zawinski's_Law
