## Background

Blog post [What Color is Your Function?](https://journal.stuffwithstuff.com/2015/02/01/what-color-is-your-function/)
(2015) by Bob Nystrom highlighted problems of async computation handling in programming language design.
It has started heated discussions on [Hacker News](https://news.ycombinator.com/item?id=23218782) 
and [Reddit](https://www.reddit.com/r/programming/comments/cpjeit/what_color_is_your_function/).

Although many solutions to this problem were suggested, none of them seemed to be a silver bullet.

## Zig's new I/O

In the [Zig Roadmap 2026](https://www.youtube.com/watch?v=x3hOiOcbgeA) stream Andrew Kelley announced
a new way of doing I/O,
and Loris Cro wrote a [Zig's New Async I/O](https://kristoff.it/blog/zig-new-async-io/) blog post describing it
in more details.

This is the example Zig code from that post, that writes `data` to two files asynchronously:

```zig
const std = @import("std");
const Io = std.Io;

fn saveData(io: Io, data: []const u8) !void {
   var a_future = io.async(saveFile, .{io, data, "saveA.txt"});
   defer a_future.cancel(io) catch {};

   var b_future = io.async(saveFile, .{io, data, "saveB.txt"});
   defer b_future.cancel(io) catch {};

   // We could decide to cancel the current task
   // and everything would be released correctly.
   // if (true) return error.Canceled;

   try a_future.await(io);
   try b_future.await(io);

   const out: Io.File = .stdout();
   try out.writeAll(io, "save complete");
}

fn saveFile(io: Io, data: []const u8, name: []const u8) !void {
   const file = try Io.Dir.cwd().createFile(io, name, .{});
   defer file.close(io);
   try file.writeAll(io, data);
}
```

Loris later claims that this approach to I/O solves function coloring... and I don't agree.

To see why, let's compare function signatures of <span style="color: #ff6666">red</span> (blocking) and
<span style="color: #7777ff">blue</span> (non-blocking) versions of `saveData` with a few modifications
that make my argument a bit more clear:

```zig
// red
fn saveData(data: []const u8) !void
fn saveFile(file: std.File, data: []const u8) !void

// blue
fn saveData(io: Io, data: []const u8) !void
fn saveFile(io: Io, file: std.File, data: []const u8) !void
```

And compare it to semantically similar functions in Node.js:

```typescript
// red
function saveData(data: Uint8Array): void
function saveFile(file: string, data: Uint8Array): void

// blue
async function saveData(data: Uint8Array): Promise<void>
async function saveFile(file: string, data: Uint8Array): Promise<void>
```

Difference is easy to spot:

  - in Node.js, <span style="color: #7777ff">blue</span> functions are marked with `async` and
return a promise (or take callback `() => void` as a parameter)
  - in Zig, <span style="color: #7777ff">blue</span> functions **take `std.Io` parameter**!

But there is a catch: with this new I/O approach *it is impossible* to write to a file without `std.Io`!

```zig
// impossible without io
fn saveData(data: []const u8) !void
```

Semantically, passing `std.Io` to every function is no different from making every Node.js function async and
returning a promise.
Zig shifts function coloring from blocking/non-blocking choice to io/non-io.

## Inevitable?

So does Zig solve a function coloring problem? Depends on who you ask:

- Yes, because function type signature is the same
- Yes, because for the caller any function is blocking
- Yes, because function can be called in both blocking and non-blocking contexts
- No, because functions doing I/O require parameter `std.Io`
- No, because I/O functions can only be called from other I/O functions
- No, because function may return the result of `std.Io.async()`, which only makes sense for functions doing async:
  ``` zig
  fn saveData(io: Io, data: []const u8) !std.Future {
      return io.async(saveFile, .{io, data, "saveA.txt"});
  }
  ```
- No, because `std.Io` interface has runtime-sensitive functions such as `asyncConcurrent` and I/O implementation might
not support it

In my opinion, function coloring is not about the syntax, not about the function's type signature -
it is about semantics and behavior of the function.
And I don't think this problem is solvable:

  - There will always be functions that are sensitive to I/O runtime they are within.
  - Some functions are inherently blocking, some inherently non-blocking.
  - Some APIs have [a way](https://man7.org/linux/man-pages/man2/fcntl.2.html) to make blocking functions behaving in
non-blocking manner.

Does this mean that the programming language needs to treat them in the same way?

## Ergonomics

But most complains related to function coloring are not that blocking and non-blocking functions need to be handled
differently.
Complains are about how *inconvenient* it is to work with these differences.
Understanding this moves the problem from computer science to the programming language design - how to make it more
convenient?

And this is where I think Zig's new I/O design does a great job.
It unifies the way of working with both execution models elegantly.
Although passing `std.Io` everywhere seems to be annoying, it seems to work well for passing `std.mem.Allocator` to any
function that allocates.
It keeps intent clear and gives great flexibility to the caller.

And as with allocation, not every bit of behavior needs to be expressed in the type signature:
function does not tell who owns allocated memory, developer either needs to read the docs or go through the code
to find out for themselves.

## Further reading

  - [What Color is Your Function?](https://journal.stuffwithstuff.com/2015/02/01/what-color-is-your-function/)
  - [Zig overview](https://ziglang.org/learn/overview/)
  - [What is Zig's “Colorblind” Async/Await?](https://kristoff.it/blog/zig-colorblind-async-await/)
  - [sans-IO: The secret to effective Rust for network services](https://www.firezone.dev/blog/sans-io)
  - [Why Algebraic Effects?](https://antelang.org/blog/why_effects/)
  - [Tokio crate documentation](https://docs.rs/tokio/latest/tokio/index.html)
  - [Async IO in Python: A Complete Walkthrough](https://realpython.com/async-io-python/)
  - [Notes on structured concurrency](https://vorpus.org/blog/notes-on-structured-concurrency-or-go-statement-considered-harmful/)
  - [Async-Await ≈ Generators + Promises](https://hackernoon.com/async-await-generators-promises-51f1a6ceede2)
