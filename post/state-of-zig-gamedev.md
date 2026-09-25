This post is my journey of rewriting web-based moto trials simulation game prototype to a native application
using Zig and WebGPU.

> I don't use LLMs/agents to produce content (text or code), only to gather information and troubleshoot external
dependency/build/tooling issues.
>
> I make games and write posts because I enjoy the process and want to share my progress,
not to increase shareholders' value

## Context

For the past 6 months or so I've been working on a [moto trials](https://en.wikipedia.org/wiki/Motorcycle_trials) simulator.
For the early prototype I picked [Three.js](https://github.com/mrdoob/three.js) (WebGL) +
[JoltPhysics.js](https://github.com/jrouwe/JoltPhysics.js) for quick iteration and out-of-the box graphics.
As a bonus I already had experience making web based games so it was a breeze.

Prototype turned out great, but issues were obvious:

  - terrible performance - updating world matrices of 200-ish objects took ~4ms, which is a quater of the whole frame
budget.
GPU was pegged to 100% with a basic scene (1440p60, 200 meshes, 4 shadow map cascades, bloom pass, FXAA), supposedly
very sane for my GTX 1050 mobile.
It also took some juggling with browser setup, meaning that the game won't be playable out of the box in every browser.
  - debugger/profiler options - chrome dev tools are fine for CPU profiling and debugging, but
[good luck](https://web.dev/articles/abouttracing) troubleshooting performance issues with WebGL.

## Stack

Becoming fed up with browser overhead the solution was simple: ~~WASM+WebGPU~~[^1] native OS game.

By that time I had a good understanding of what I need from the game stack:

  - bare-metal performance
  - down-to-machine code debugging and profiling
  - excellent C interop and rich ecosystem of gamedev oriented libraries
  - full control over dependencies' source
  - fast compilation and great tooling
  - cross-compilation to at least Linux and Windows

### Language

Considered options:

  - C++ - while being the go-to option for gamedev, I have zero desire to read it and negative infinity desire to write it myself
  - C - good minimalist choice ticking all boxes, but standard library is [terrible](https://nullprogram.com/blog/2023/02/11/),
macro system is [too forgiving](https://edryd.org/posts/macro-dangers/), [sentinel strings](https://queue.acm.org/doi/10.1145/2001562.2010365),
and writing strings/arrays from scratch every time does not spark joy to me
  - Rust - or as I say [C++++](https://medium.com/@daxx5/rust-is-becoming-the-new-c-complicated-arrogant-and-inevitable-ba3e7c577529),
is nice when writing simple single threaded CLI tools, but quickly falls apart once you introduce
[concurrency](https://tweedegolf.nl/en/blog/237/async-rust-never-left-the-mvp-state/),
[structs with references](https://www.reddit.com/r/rust/comments/18e3oq0/why_do_lifetimes_need_to_be_leaky/),
[C interop](https://lwn.net/Articles/1006805/).
I also played with [Bevy](https://github.com/bevyengine/bevy) before and didn't like it at all.
  - Odin - I'm not a fan of batteries-included standard library, but don't listen to me as I never tried it.
  - Jai - might be the best language of all time, but it doesn't matter as it's not public yet
  - Zig - ticks all boxes and [I had pleasure working with](https://github.com/ivanjermakov/hat) in the past.
Having similar semantics to C/C++ I was sure it would be a great fit for game development.

#### Graphics API

Picking graphics API these days might be even harder than picking a language, but it's more
[forced](https://www.youtube.com/watch?v=7bSzp-QildA&t=113s) by target OSes.

Linux and Windows native support narrows the list down to OpenGL, Vulkan, and WebGPU.
As OpenGL is [quite a mess](http://www.joshbarczak.com/blog/?p=154) for beyond-tutorial graphics programming
and Vulkan is [more verbose](https://github.com/SaschaWillems/Vulkan/blob/master/examples/triangle/triangle.cpp) than
enterprise Java (which I've been writing professionaly for my whole career), I landed on WebGPU.
In fact, native WebGPU support multiple backends making it a nice abstraction over Vulkan.
And similar to Zig, I also had [great expirience](https://github.com/ivanjermakov/moonlight) working with browser
implementation of WebGPU (even though it barely worked on my standard Firefox+Linux setup).

There are two major WebGPU native implementations, [wgpu](https://github.com/gfx-rs/wgpu) and [dawn](https://dawn.googlesource.com/dawn).
I picked wgpu but we will discuss later what this duopoly means for the ecosystem.

#### Physics engine

As my prototype was made using [Jolt](https://github.com/jrouwe/JoltPhysics) I initially considered using it here too,
but [legend himself](https://x.com/erin_catto) dropped [Box3D](https://box2d.org/posts/2026/06/announcing-box3d/) out of nowhere.
Not just that, but its features and [performance](https://youtu.be/HLfGmcX4vCA) seemed not worse than Jolt's.
So I decided to give Box3D a try.

#### Other

For handling windows and input I just picked [GLFW](https://www.glfw.org/) via [zglfw](https://github.com/zig-gamedev/zglfw)
wrapper and barely had any issues with it.

I rely on GLTF models exported from Blender for everything scene related (rider, bike, scenery, obstacles),
[cgltf](https://github.com/jkuhlmann/cgltf) wrapped via [zmesh](https://github.com/zig-gamedev/zmesh) worked like a charm.
Images are loaded via [stb_image](https://github.com/nothings/stb)/[zstbi](https://github.com/zig-gamedev/zstbi).

[Tracy](https://github.com/wolfpld/tracy) is a must have for good instrumentation and sampling,
wrapped via [ztracy](https://github.com/zig-gamedev/ztracy).

[zalgebra](https://github.com/kooparse/zalgebra) is a nice little library for math primitives absolutely necessary for game development.

Making GUI from scratch is a fun challenge, but for debug menus [imgui](https://github.com/ocornut/imgui) is well established.
Wrapped via [zgui](https://github.com/zig-gamedev/zgui).

I will also need audio API which will likely be [FMOD](https://www.fmod.com), but I haven't tackled sfx yet.

## Difficulties

### Outdated

Because Zig is not 1.0 [yet](https://github.com/ziglang/zig/milestone/2), breaking changes are quite common and quite often.
Most Zig libraries are not fast enough to catch up with the latest 0.x release, but luckily they usually have an already
open PR to bump the project's Zig version.

### Missing Zig bindings

While Zig has excellent C interop, Zig has its own "idiomatic" way to writing libraries which is quite different from C.
Which is why it's common to have a wrapper libraries to convert between the two.

I managed to find two Box3D wrappers for Zig, just-deleted [box3d-zig](https://github.com/Foundation42/box3d-zig) (classic GitHub) and
[zigbox3d](https://github.com/sluggga/zigbox3d).
While one of them had full box3d header, another had nicer Zig bindings.
I shamelessly combined the two into [zbox3d](https://github.com/substepgames/zbox3d) library.

There are probably countless of great C libraries that I haven't even considered for my game just because it has zero
exposure in Zig ecosystem.
Which is a shame but also an opportunity.

### zalgebra OpenGL assumptions

Camera perspective functions in zalgebra assume that [NDC](https://learnopengl.com/Getting-started/Coordinate-Systems)
depth is in range [-1, 1], while WebGPU and all other modern graphics APIs have it in range [0, 1].

### Zig Vector != Zig array

What will the following expression produce?

```zig
@sizeOf(@Vector(2, u32))
@alignOf(@Vector(2, u32))
```

Any definite answer would be wrong, because [it depends on the backend and `OptimizeMode`](https://github.com/ziglang/zig/issues/24179).
On `x86_64-linux-gnu` it gives `8 8` in Debug and `4 4` in Release.

I naively used `za.Vec2U`s in my uniforms struct and was quite surprized to see my shaders work in debug but break in
release mode.

Worst part is that there is no concise fix for it. I [had to](https://github.com/kooparse/zalgebra/commit/ddbc799409a3e7a484a3986ebc7e30fdd9623f82)
rewrite all vectors to use arrays instead (no significant performance difference).

It was extremely easy to debug though, big thanks to [RenderDoc](https://renderdoc.org/).
All I had to do is to sample two frames into two RenderDoc windows and compare uniform buffers bound to vertex shaders[^2].

### Prebuilt binaries in `build.zig.zon`

[wgpu_native_zig](https://github.com/bronter/wgpu_native_zig) relies on prebuilt binaries of a fixed (1 year outdated)
`wgpu-native` version.
While I understand the convenience, I would like to have full control over dependency sources and have a way to update it.
Of course `build.zig` needed an [overhaul](https://github.com/bronter/wgpu_native_zig/commit/e2ac36fcab0b54cb63b4614cfc4d806ec8c19b5c)
but was not that bad.
Compiling it from scratch unfortunately can't be handled by Zig's toolchain as `wgpu` is written in Rust,
but there is nothing `addSystemCommand` can't do:

```zig
const make_target = switch (optimize) {
    .Debug => "lib-native",
    else => "lib-native-release",
};
var cmd = b.addSystemCommand(&.{ "make", make_target });
cmd.cwd = b.path("lib/wgpu_native_zig/lib/wgpu-native");
b.getInstallStep().dependOn(&cmd.step);
```

Would be nice to have this code in `wgpu_native_zig`'s instead of my game's `build.zig`, but I don't think it's
possible to add a dependency to `Module` directly.

### wgpu-native update pain

Since `wgpu_native_zig` was using old prebuilt binaries of `wgpu-native` I wanted to update it to the latest version
built from source.
Of course it broke Zig bindings because of breaking changes in `webgpu.h`, without an obvous way around it.
I had to go through all `[breaking]` commits in [webgpu-headers](https://github.com/webgpu-native/webgpu-headers)
project and [fix them](https://github.com/bronter/wgpu_native_zig/commit/e2ac36fcab0b54cb63b4614cfc4d806ec8c19b5c)
one by one.

Updating `webgpu.h` also made it necessary to update Box3D's WebGPU backend, which turned out to be quite straightforward,
except I [somehow managed](https://github.com/zig-gamedev/zgui/commit/670df2b632ad41870797df34d6f9933d13efc052)
to include two different versions of WebGPU headers used by `wgpu-native` and imgui.
Troubleshooting it was especially painful because ABI mismatch only surfaces at runtime as segfaults or failed
wgpu pipeline/bind group/pass validations.

### Not using submodules

Many projects just raw dog external dependencies into their project without any reference to its version or origin.
On a bright side, it was a great opportunity to learn how to properly use [git submodules](https://git-scm.com/book/en/v2/Git-Tools-Submodules)
to vendor external deps.

### zig + imgui + wgpu = big misery

Using `imgui` alone is not as straightforward thing as I thought.
There are custom options to use `docking` version, include plot, knob, freetype support.
And on top of that you have window system + graphics API combo (glfw + wgpu in my case).
It wouldn't be that bad if `zgui` [didn't assume](https://github.com/zig-gamedev/zgui/blob/48253e73827ecc50fa60f523887fdbe93bbf61a6/build.zig#L273)
I'm using `zdawn` backend.

I had to rewrite its `build.zig` to add `-DIMGUI_IMPL_WEBGPU_BACKEND_WGPU` flag and include `webgpu.h` together with
wgpu-specific `wgpu.h`.

### glfw + wgpu = minor inconvenience

I said that I barely had any issues with GLFW, but barely is not none.
GLFW [does not know by itself](https://github.com/eliemichel/glfw3webgpu) how to create surface when used with WebGPU. 
I think it's reasonable, except `wgpu_native_zig` exposes 6 functions depending on what window system is
*without exposing one function* to use in all cases.

```zig
pub inline fn surfaceDescriptorFromAndroidNativeWindow(descriptor: MergedSurfaceDescriptorFromAndroidWindow) SurfaceDescriptor;
pub inline fn surfaceDescriptorFromMetalLayer(descriptor: MergedSurfaceDescriptorFromMetalLayer) SurfaceDescriptor;
pub inline fn surfaceDescriptorFromWaylandSurface(descriptor: MergedSurfaceDescriptorFromWaylandSurface) SurfaceDescriptor;
pub inline fn surfaceDescriptorFromWindowsHWND(descriptor: MergedSurfaceDescriptorFromWindowsHWND) SurfaceDescriptor;
pub inline fn surfaceDescriptorFromXcbWindow(descriptor: MergedSurfaceDescriptorFromXcbWindow) SurfaceDescriptor;
pub inline fn surfaceDescriptorFromXlibWindow(descriptor: MergedSurfaceDescriptorFromXlibWindow) SurfaceDescriptor;
```

Fine... I'll ~~write it myself~~ steal it from
[here](https://github.com/spencrc/hello-triangle-zig-wgpu/blob/493c21a1226edbabfdf58383c319432d7984e7e2/src/glfw_wgpu.zig#L56):

```zig
pub fn createSurface(instance: *wgpu.Instance, window: *zglfw.Window) !*wgpu.Surface {
    switch (builtin.target.os.tag) {
        .macos => return createMetalSurface(instance, window),
        .windows => return createWindowsSurface(instance, window),
        .linux => return switch (zglfw.getPlatform()) {
            .x11 => createX11Surface(instance, window),
            .wayland => createWaylandSurface(instance, window),
            else => return error.PlatformUnsupported,
        },
        else => return error.PlatformUnsupported,
    }
}
// ...
// skipped for brevity
```

### Box3D world snapshots

While I was very enthusiastic about using Box3D, the time has come to implement physics world save/restore to have
the bike reset to the latest checkpoint on failure.
To make it work I need:

  1. Create bodies, shapes, constraints, sensors for the scene
  2. Save the world snapshot
  3. Once it's time for reset, restore existing world to the saved snapshot

I read this line in documentation and immediately became 75% less enthusiastic:

> This machinery is currently internal, used only to seed and replay recordings.
Box3D does not yet expose a standalone save-state / restore API.

Good news I own my dependency sources and can implement it myself, how hard can it be?

Existing record/replay system in Box3D is focused on providing devs with a debug replay of a simulation.
Naively deserializing it into the same world is not possible with current API, you read replay world ID from deserialized replay object.
But updating Box3D to accept existing world breaks simulation and existing body/joint/shape references.

Save/restore is not a thing yet because serialized state should be self-contained, meaning it should reproduce the whole world exactly from bytes alone.
There is an [open issue](https://github.com/erincatto/box3d/issues/134) for it:

> There needs to be some optimizations to make this reasonable.
The current snapshot embeds all collision meshes.
There should be some sort of context to keep the data size more reasonable.

But wait! My use case is different. I don't create/remove shapes after snapshot save, so it's not a big deal?

Box3D privately uses this snapshot functions to drive its replay system

```c
int b3SerializeWorld( b3World* world, b3RecBuffer* buf, b3Recording* rec );
bool b3DeserializeIntoShell( const uint8_t* data, int size, b3World* world, b3RecReader* rdr );
```

All I had to do is to make two similar functions that basically do the same thing just
[don't include shapes](https://github.com/erincatto/box3d/compare/main...substepgames:box3d:main) in the snapshot:

```c
int b3SaveSnapshot( b3WorldId worldId, b3RecBuffer* buf );
bool b3RestoreSnapshot( const uint8_t* data, int size, b3WorldId worldId );
```

...and expose them in `box3d.h` and `zbox3d`.

### Jolt side track

Talking about patching Box3D like it was easy is not really true and I procrastinated for two days not wanting
to understand Box3D's snapshot serialization logic.
Another possible approach was to switch back from Box3D to familiar Jolt as I'm sure it has all functionality I need.

Did you know C++ library can't be used directly as C-compartible ABI? It needs a shim like any other language.
Thinking more about it it makes perfect sense since C++ has a bazillion features foreign to ABI.

Jolt allegedly has [Zig bindings](https://github.com/zig-gamedev/zphysics) and 3 ([three!](https://github.com/jrouwe/JoltPhysics/blob/7ab5be3228a8677268d648a26f753ef507f4b96a/README.md?plain=1#L157))
more providing C bindings. Such diversity!

Let's use zphysics then. Its [C bindings](https://github.com/zig-gamedev/zphysics/blob/main/libs/JoltC/JoltPhysicsC.h) seem to be hand-written.
Oops, it's missing basic building blocks like constraints other than fixed.

What about [JoltC](https://github.com/amerkoleci/joltc), it has more stars on GitHub® meaning that it must be objectively higher quality.
Good, it has all constraint types, even `PhysicsSettings`, but no `PhysicsSystem.restoreState`...

Same for other two C bindings repos, they lack some of the Jolt's interface that I use.

Even if they had it I would still need to write Zig bindings for it by hand (great use case for LLMs by the way!)
or spend another week writing bindings generator.

## Current state of the game

Now that I can breathe out and write my down journey, game is in a good state.

Performance-wise, I get 120fps in the same scene setup where I had a GPU bottleneck in a browser version,
but this time with 4xMSAA instead of FXAA. Without AA I get 210fps.
Surprisingly CPU-bound this time, spending a hefty amount of time submitting command buffers to the GPU.
Might experiment with a render thread architecture and triple buffering once I get bored implementing gameplay.

Haven't tried Windows[^3] build yet, but expecting a bunch more problems building dependencies from source.

## Things I learned

  - Owning dependencies is very powerful. Just going to definition and tweaking foreign code feels good.
  - Tracy is very useful.
  - RenderDoc is very useful.
  - gdb is often better than `printf`s, but I can't wait for Linux support in the [RAD Debugger](https://github.com/EpicGames/raddebugger).
  - C++ library cannot be used directly as C ABI and needs shims like any other language.
  - `wgpu` and `dawn` while targeting the same API are not interchangable.
  - Writing `build.zig` from scratch is not that hard, but breaking changes twice a year is still annoying.
  - Making sure size and alignment of structs across CPU/GPU boundary is not always trivial.
  - CPUs are stupidly fast if you compile your code natively.
  - Big part of engineering is struggling with other people's decisions

## Conclusion

Using Zig for gamedev and anything else is a great choice if you know what you're doing.
Or at least know how to navigate encountered issues.

Native WebGPU has great performance and nice yet flexible API.
I found it quite annoying to have to declare `BindGroupLayout` for each `BindGroup`[^4], but luckily I only need to
do this once when initing graphics.
Validators in wgpu are capable of detecting many misconfigurations early (on first frame).

High quality game physics is finally available for indie devs with the release of Jolt and now Box3D.

While web is an amazing platform for delivering content and providing service, it still lacks
performance and tooling needed for joyfull videogame development.

## Be useful

> You did all this hard work and didn't contribute to the projects you complained about so much?

  - All my work is in [public forks](https://github.com/substepgames).
  - This post might guide lost souls confused about certain aspects of working with these technologies.
  - Most of the fixes I did are quite specific to my use case and not ready to be immediately useful to others.
Yet another power of owning dependencies!

## Credit

Maddest props to [zig-gamedev](https://github.com/zig-gamedev) project and all its contributors.
While this post might be a bit harsh towards their work, without it my journey might've never started or took N times
more time and M times more energy.

## Self promotion

If you would be interested in playing trials game and other hand-crafted games I'll make in the future,
I invite you to the [Substep Game's](https://substepgames.com) [discord](https://discord.gg/wtcZ5q5zHN).

## Further reading

  - [Zig Build System](https://ziglang.org/learn/build-system/)
  - [Git - Submodules](https://git-scm.com/book/en/v2/Git-Tools-Submodules)
  - [Introducing Box3D (video)](https://youtu.be/jr_Fzl2XwKU)
  - [It's Not About The API - Fast, Flexible, and Simple Rendering in Vulkan (video)](https://youtu.be/7bSzp-QildA?si=kE-5tAC5mo8l3L8O)

<br>

[^1]: while it might be good for performance, tooling is non-existent, and writing/generating JS shims is not fun.
[^2]: would be not too hard to troubleshoot this CPU-side with `std.mem.asBytes(&uniforms)`.
[^3]: imagine having a Windows machine to play games in 2026.
[^4]: now I understand why everyone is prasing "bindless" in Vulkan.
