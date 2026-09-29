# gomuks.el

A [Gomuks](https://github.com/gomuks/gomuks) (i.e. Matrix) client inside Emacs.
Displays a full-window room list, compact chats, and a buffer to write messages in.

This needs Emacs 28.1+, `curl`, and a running, logged-in Gomuks backend v26.08 or
newer. Log in through Gomuks first; account setup isn't part of this package.

## Getting started

If you're trying it from a local checkout, add the directory containing
`gomuks.el` to your Emacs load path:

```elisp
(add-to-list 'load-path "/path/to/gomuks.el")
(require 'gomuks)
```

Or you can also, from inside Emacs, `M-x load-file RET /path/to/gomuks.el/gomuks.el RET`

For Doom, add this to `packages.el`:

```elisp
(package! gomuks
  :recipe (:host github :repo "soliprem/gomuks.el"))
```

Then run `doom sync` and restart Emacs. Either way, start with `M-x gomuks`.
You'll get a home page with your recent rooms. Press `RET` on one to open it;
the chat goes in the upper window, with a composer below. `q` from the home page
restores your previous window layout.

The backend credentials Gomuks created on first start are separate from your
Matrix login. For a login that survives an Emacs restart, put them in an
`auth-source` backend. For example, in `~/.authinfo.gpg`:

```text
machine localhost port 29325 login YOUR_USER password YOUR_PASSWORD
```

`gomuks.el` looks up the host and port in `gomuks-backend-url`. If it finds no
entry, it asks as before and keeps the password only for this Emacs session.
`M-x gomuks-change-credentials` prompts for an override in the current session;
update the stored entry too if the credentials really changed. Run
`M-x gomuks-disconnect` to close the event stream and forget the in-memory
password.

## Chatting

Each room has a multiline draft that survives navigation and failed sends.
Replies and edits have separate drafts; thread drafts send into the thread.
Files and clipboard images queue in the composer. They send as separate messages
in order, with the draft text on the last attachment. If one fails, the unsent
files and draft text stay put. Messages use room display names; hover over a
sender for the full Matrix ID. Links in messages are clickable.

Search covers history Gomuks has fetched. Small images preview inline in
graphical Emacs. Audio playback requires `mpv`. GIF URLs must point directly
to an image, rather than a web page.

## Keys

| Mode | Key | Action |
| --- | --- | --- |
| Home | `RET` | Open room |
| Home | `g` | Reconnect |
| Home | `q` | Restore previous windows |
| Room | `C-c C-s` | Focus composer |
| Room | `C-c C-r` | Reply at point |
| Room | `C-c C-e` | Edit at point |
| Room | `C-c C-+` | React at point |
| Room | `RET` on a reaction | Show who used that reaction (`d` removes yours; `q` closes) |
| Room | `C-c C-d` | Redact at point |
| Room | `d d` | Redact the message at point, after confirmation |
| Room | `C-c C-m` | Mark read at point |
| Room | `C-c C-t` | Open thread at point |
| Room | `C-c C-j` | Follow reply at point |
| Room | `C-c C-f` | Search this room |
| Room | `C-c C-a` | Upload a file |
| Room or composer | `C-c C-p` | Send a local image as a sticker |
| Room or composer | `C-c C-g` | GIF/WebP file or direct HTTPS image URL (stage in composer) |
| Room | `o` / `C-c C-o` | Open attachment at point |
| Room | `C-c C-SPC` | Pause or resume audio (EMPV) |
| Room | `C-c <` / `C-c >` | Seek audio back or forward five seconds (EMPV) |
| Room | `D` / `C-c C-w` | Save attachment at point |
| Room | `C-c C-u` | Copy sender's full Matrix ID |
| Room | `M-p` | Load older messages |
| Room | `b` / `q` | Go back |
| Composer | `C-c C-c` | Send draft |
| Composer | `C-c C-a` | Add a file to the draft |
| Composer | `C-c C-d` | Remove a queued attachment |
| Composer | `C-c C-o` | Preview a queued attachment (`d` removes it) |
| Composer | `C-c C-e` | Find and insert an emoji |
| Composer | `C-c C-v` | Add a clipboard image to the draft |
| Composer | `C-c C-k` | Return to chat, keeping the draft |
| Search | `RET` | Open a result with nearby messages |
| Search | `n` | Load more results |
| Search | `b` / `q` | Return to the room |
| Search result context | `b` | Return to the results |
| Attachment preview | `RET` | Open in the system viewer |
| Attachment preview | `d` / `q` | Remove attachment / close preview |

The room title and topic stay visible above the timeline while you scroll.

Evil starts the home page and chat in normal state, and the composer in insert
state. Movement and search behave as you'd expect. On the home page, `o` opens a
room. In a chat, `i` focuses the composer; `r`, `E`, `R`, `x`, and `M` reply,
edit, react, redact, and mark read. `p` loads history, `T` opens a thread, `J`
follows a reply, and `a` attaches a file. `o` and `D` open and save attachments;
`U` copies the sender ID. Use `g r` to reconnect. The `C-c` keys above work with
Evil too. In Doom, the same actions are also under your configured localleader:
`C-c C-r` becomes `<localleader> r`, and the audio controls use
`<localleader> SPC`, `<localleader> <`, and `<localleader> >`.
Doom uses its alternate localleader in the composer’s insert state.
These are ordinary mode-map bindings: change the `C-c` keys with `define-key` on
`gomuks-room-mode-map`, or the Doom keys with `map! :map gomuks-room-mode-map
:localleader` in your config after Gomuks loads.

## Notifications and limits

Desktop notifications are on when Emacs supports them. Set
`gomuks-desktop-notifications` to `nil` if you don't want them. Echo area previews
appear only while a Gomuks buffer is selected. Set
`gomuks-echo-area-notifications` to `t` to show them everywhere, or `nil` to
hide them entirely.

This still renders plain text message bodies. Image previews work, but rich
HTML, room management, typing indicators, and Matrix account setup aren't in
the Emacs interface yet.

To run the tests from this directory:

```sh
emacs -Q --batch -L . -l gomuks-tests.el -f ert-run-tests-batch-and-exit
```
