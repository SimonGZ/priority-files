# Priority Files

Priority Files is an Emacs package designed to allow quick access to essential files within a Projectile project. The files are defined in a `.priority-files` list.

## Why?

I often work on screenplays in Emacs, which means I'm frequently jumping back to the same file over and over again from different notes and outlines. I also find that if I'm working on something like a React project, I'll be jumping to the main App.tsx over and over.

Projectile already has great tools for jumping between many files, but I like to be able to assign a keyboard shortcut that always jumps to the file I want.

## Features

- **Jump to Priority Files**: Quickly access files defined as priorities in your project.
- **Cycle Through Priority Files**: Seamlessly navigate through the list of priority files.
- **Edit Priority List**: Easily create or modify your list of priority files directly within Emacs.
- **Add Current File**: Quickly add the file you're currently editing to your priority files list.

## Installation

I use Doom Emacs which uses straight.el. I put the following in my `packages.el`:

```lisp
(package! priority-files
  :recipe (:host github
           :repo "SimonGZ/priority-files"))
```

Then, I run `doom sync` on the command line and restart Emacs for the changes to take effect.

## Usage

1. **Define Priority Files**: In the root of your project, create a `.priority-files` file listing the relative paths of files you consider priorities, one per line. Blank lines and lines starting with `#` are ignored.

### Dynamic entries

In addition to plain paths, `.priority-files` understands `latest` directives that resolve at runtime. This allows you to jump to "the most recent" file that matches a pattern without editing your list every time.

```
# Plain entries
docs/overview.org

# Use the newest markdown note under notes/
latest notes/*.md

# Prefer the most recently created journal entry
latest:created journal/**/*.md
```

The syntax is `latest[:MODE] GLOB`, where `MODE` defaults to `modified`. Valid modes are:

- `modified`: pick the file with the most recent modification time
- `created`: pick the file with the newest birth/creation time; if the filesystem does not track birth times, the directive automatically falls back to modification time.
- `filename`: pick the file that is last in alphabetical order (useful for date-based filenames like `YYYYMMDD`); sorts matches by filename in descending order.

Patterns are resolved with `file-expand-wildcards`, so you can use globbing such as `*.md` or `**/*.org`. Only files within the project root are considered.

2. (OPTIONAL) **Keybindings**: Add keybindings to your `config.el` to integrate Priority Files into your workflow:

```lisp
(map! :leader
      (:prefix ("p" . "project")
       :desc "Jump to top priority file" "j" #'priority-files-next
       :desc "Cycle through priority files" "J" (cmd! (priority-files-next t))
       :desc "Edit priority files" "E" #'priority-files-edit
       :desc "Add current file to priority files" "A" #'priority-files-add-file))
```

### Commands

- `priority-files-next`: Jump to the top priority file. If already viewing the top file, jumps to the next one. With a prefix argument (`C-u`), cycles through all priority files.
- `priority-files-edit`: Opens the `.priority-files` in the current project for editing or creation.
- `priority-files-add-file`: Adds the file you're currently editing to the `.priority-files` list. If the list doesn't exist yet, it creates one in your project root. This is a convenient way to build your priority files list as you work.

## License

This project is licensed under the MIT License. See the [LICENSE](https://github.com/SimonGZ/priority-files/blob/main/LICENSE) file for details.
