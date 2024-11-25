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
