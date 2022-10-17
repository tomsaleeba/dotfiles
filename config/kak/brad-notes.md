Brad:
I've been exclusively using kakoune for ~ a year now if you have any questions :slightly_smiling_face:

Tom:
@brad I do :star-struck:. They're all going to center around "how I do get some
behaviours from vim that I like". I'm assuming you know vim too.
I'm looking back at my notes from when I tried kak, which was last Nov so
defaults might've changed.
what do you use for splits/tabs?
- do you have a way for search highlights to work over all kakoune instances. In
  vim if you have highlight turned on, you can search in one split and matches
  in other splits will also highlight. This doesn't happen out of the box for
  kak because splits are different instances.
- can you show the total number of matches for a search?
- do you have a way to list all open buffers and the contents of registers?
- do you change your cursor to indicate normal/insert mode?
- do you have a shortcut that works like :qa? Now I think about it I could find
  and kill the processes :thinking_face:
- do you use fzf? If so, can you
- open splits
- open new tabs
- focus tabs that already have the selected buffer
- do you have something to simulate set textwidth=X that will wrap as you write?
  I know I can use par or fmt once in normal mode but I like it wrapping as I
  go.
- do you have a way to quick switch to the last buffer, like C-6.

Brad:
> what do you use for splits/tabs?
I use i3wm, but I've been slowly working on having it fallback to kitty for
environments I can't use i3 (MacOS, Chromebook, etc). I will probably continue
using a VM for work though even if I get that working
> do you have a way for search highlights to work over all kakoune instances. In
> vim if you have highlight turned on, you can search in one split and matches
> in other splits will also highlight. This doesn't happen out of the box for
> kak because splits are different instances.
I'm glad you asked this question, it made me realise my highlights weren't
working on this machine (missing bold/italic fonts). I use the following:
hook global RegisterModified '/' %{ add-highlighter -override global/search regex "%reg{/}" 0:+b }
which causes anything in the search register / to become bolded. If you have
multiple clients attached to the same kakoune session, they will share that
highlighting. If you open up multiple kakoune instances it wouldn't though,
so I'd recommend taking advantage of the client/server architecture kakoune
uses.
can you show the total number of matches for a search?
I'm assuming you mean a / search and not a :grep search, then... kind
of. Kakoune pushes you to use selections instead of searches, so instead of
using / to search we'd use % to highlight the entire buffer, and then s
to search within the current selection. Then the bottom right corner will
show how many selections you have, which is how many terms matched your
search
do you have a way to list all open buffers and the contents of registers?
I'm not sure if you mean in the terminal or in kakoune. In kakoune it's easy:
- :echo %val{buflist} to list buffers
- :echo %reg{p} to list contents of a register
outside of kakoune, there is a great project by a friend of mine that I
highly recommend: [kakoune.cr](https://github.com/alexherbo2/kakoune.cr)
using this CLI tool you can control kakoune from the terminal, which is
very understated how valuable it is. They have a playground where they demo
reproducing the logic of what was previously a fairly complicated plugin
to add auto-pairs to kakoune using jq and a couple lines of bash.
do you change your cursor to indicate normal/insert mode?
If you mean like a beam, then you're not going to find that in kakoune as
it breaks the "everything is a selection" concept. I think you can change
the highlighted color of the cursor in different modes, but it never really
bothered me so I didn't look into it at all
do you have a shortcut that works like :qa? Now I think about it I could find and kill the processes :thinking_face:
If you are leveraging the client/server architecture, you can run :kill
to kill the session and hence all clients associated with that session.
I actually have a [script](https://github.com/Parasrah/dotfiles/blob/master/scripts/k)
that will create a session based on which directory you're in.
do you use fzf? If so, can you open splits?
I do use fzf, using kakoune.cr (mentioned above). I suspect you could tie
it together to work with splits, it isn't something I personally used in
vim either though (I always create the split first and then do things). One
thing I really like about kakoune is how easy it is to tie things together
yourself, but most users do use tmux (I don't), so it is fairly well
integrated with tmux out of the box, and I think another authors plugin: [fzf.kak](https://github.com/andreyorst/fzf.kak)
properly splits with tmux out of the box (but don't quote me on that)
can you open tabs?
Could I? Sure, I could open tabs in i3. Have I set that up? No, I personally
rely on just creating a split and converting it into tabs using my i3
keybindings. I do however use kitty overlay tabs a lot, to overlay the
current window. I use that for stuff like nnn, so if I press - and use nnn
to select a file, kakoune will change to that file. An old habit from my vim days

Brad:
can you focus tabs that already have the selected buffer?
Even in vim I always kind of relied more on the buffer list, so I haven't configured something like that personally, but I can already imagine how I could do that:
- use kcr to list sessions/clients
- find the client in the current session with that buffer open (if any) using jq
- if you found a buffer, send that client a command to take focus
- otherwise, open in current client as normal
You can see the glue that I use for fzf [here](https://github.com/Parasrah/kakoune.cr/blob/master/share/kcr/commands/fzf/kcr-fzf-files),
and how simple it is (kudos of my friend again). That's a common theme with kakoune, glue unix tools
together to come up with a minimal, simple solution
do you have something to simulate set textwidth=X that will wrap as you write? I know I can use par or fmt once in normal mode but I like it wrapping as I go.
That's a good question, I don't even know, but I can see how that would
be handy. You can set hooks when you insert characters, so I imagine it's
possible, it's not something I've ever considered though (I use = a lot
for that)
do you have a way to quick switch to the last buffer, like C-6.
There is a :buffer-previous command if that's what you mean?
In all, it's definitely not as feature-ful as vim out of the box, but it
makes it really easy to glue other tools together with kakoune. Personally
I really like that approach as my dev env is pretty unique/personal to me,
and it means I can seamlessly fit kakoune into that. I think the biggest thing
I miss though is a good git diff w/ splits, vim-fugitive was very good


Tom:
@brad thank you so much for such comprehensive answers. I also run i3wm with
kitty and I'm thinking about how to move that workflow to my R&P issued Macbook.
A VM was my most recent idea but I'm yet to test if I can run a multimonitor VM.
I have much to learn from your dotfiles :heart_eyes:.

I'm definitely going to give kak another try.

I may be back once I digest all your answers.


Brad:
For sure, happy to help and answer any other questions you might have in the future! As far as the multi-monitor support, I see a button for it on VMWare Fusion but can't try it out as I don't personally use multiple monitors. Also if you do decide to go that route and kitty is a must have for you like it is for me, steer clear of parallels. It doesn't have support for an OpenGL capability required by kitty
