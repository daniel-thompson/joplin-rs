# Quickstart

## Setting up joplin

1. Open the **Options**
2. Navigate to **Web clipper**
3. Find *Step 1: Enable the clipper service* and click
   **Enable Web Clipper Service**
4. Ignore *Step 2: Install the extension*
5. Find *Advanced options* and click **Copy token**

## Setting up joplintool

Edit $HOME/.joplin/Joplin.toml and set it up as follows:

~~~toml
token = "<paste your API token here>"
~~~

Note that if you are using joplintool to access a remote joplin
instance you can set the `uri` key in the TOML file to handle that.

## Filing TODOs onto a kanban board

By default the `todo` command files new tasks as TODO items in the
**TODO** notebook. If you prefer to manage tasks using the kanban
plugin then set the `kanban_board` key to the title of the board
note:

~~~toml
kanban_board = "Work kanban"
~~~

When `kanban_board` is used the `diary` command will also migrate tasks from
the "Done" column on the board. It is also possible to migrate existing
TODO items using `todo --migrate`.
