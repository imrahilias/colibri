-- this plugin provides cross-instance yank ability, which means you
-- can yank files in one instance, and then paste them in another
-- instance:
require("session"):setup {
   sync_yanked = true,
}

--- BROKEN
require("starship"):setup()

-- git needs nightly:
require("git"):setup()
--THEME.git = THEME.git or {}
--THEME.git.modified = ui.Style():fg("blue")
--THEME.git.deleted = ui.Style():fg("red"):bold()


-- fast but inaccurate:
-- require("mime-ext"):setup {
--    -- Expand the existing filename database (lowercase), for example:
--    with_files = {
--       makefile = "text/x-makefile",
--       -- ...
--    },
--    -- Expand the existing extension database (lowercase), for example:
--    with_exts = {
--       mk = "text/x-makefile",
--       -- ...
--    },
--    -- If the mime-type is not in both filename and extension databases,
--    -- then fallback to Yazi's preset `mime` plugin, which uses `file(1)`
--    fallback_file1 = false,
--                           }


-- show user/group of files in status bar:
Status:children_add(function()
      local h = cx.active.current.hovered
      if h == nil or ya.target_family() ~= "unix" then
         return ui.Line {}
      end

      return ui.Line {
         ui.Span(ya.user_name(h.cha.uid) or tostring(h.cha.uid)):fg("magenta"),
         ui.Span(":"),
         ui.Span(ya.group_name(h.cha.gid) or tostring(h.cha.gid)):fg("magenta"),
         ui.Span(" "),
      }
end, 500, Status.RIGHT)

-- custom shell:
require("custom-shell"):setup({
    history_path = "default",
    save_history = true,
})

-- fg customisations, do not work for the window `--preview-window
-- 'right:50%'`, that has to be changed in the plugin `main.lua`!
require("fr"):setup {
   fzf = [[--color=dark --no-info --no-separator --color='prompt:magenta' --inline-info --border=none --preview-window=border-none --layout=reverse --ansi]],
   --rg = "--colors 'line:fg:red' --colors 'match:style:nobold'",
   rg = "--hidden --column --line-number --no-heading --color=always --smart-case",
   bat = "--style 'header,grid' --number --color=always",
   rga = {
      "--follow",
      "--hidden",
      "--no-ignore",
      "--glob",
      "'!.git'",
      "--glob",
      "!'.venv'",
      "--glob",
      "'!node_modules'",
      "--glob",
      "'!.history'",
      "--glob",
      "'!.Rproj.user'",
      "--glob",
      "'!.ipynb_checkpoints'",
   },
   rga_preview = {
      "--colors 'line:fg:red'"
         .. " --colors 'match:fg:blue'"
         .. " --colors 'match:bg:black'"
         .. " --colors 'match:style:nobold'",
   },
                    }
