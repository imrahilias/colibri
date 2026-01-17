-- this plugin provides cross-instance yank ability, which means you
-- can yank files in one instance, and then paste them in another
-- instance:
require("session"):setup
{
   sync_yanked = true,
}


-- Never tested.
-- require("sshfs"):setup({
--       -- Mount directory
--       mount_dir = "~/mnt",

--       -- Password authentication attempts before giving up
--       password_attempts = 3,

--       -- SSHFS mount options (array of strings)
--       -- These options are passed directly to the sshfs command
--       sshfs_options = {
--          "IdentityFile=/home/m/.ssh/id_rsa.pub",
--          "reconnect",                      -- Auto-reconnect on connection loss
--          "ConnectTimeout=5",               -- Connection timeout in seconds
--          "compression=yes",                -- Enable compression
--          "ServerAliveInterval=10",         -- Keep-alive interval (15s × 3 = 45s timeout)
--          "ServerAliveCountMax=3",          -- Keep-alive message count
--          -- "dir_cache=yes",               -- Enable directory caching (default: yes)
--          -- "dcache_timeout=300",          -- Cache timeout in seconds
--          -- "dcache_max_size=10000",       -- Max cache size
--          -- "allow_other",                 -- Allow other users to access mount
--          -- "uid=1000,gid=1000",           -- Set file ownership
--          -- "follow_symlinks",             -- Follow symbolic links
--       },

--       -- Picker UI settings
--       ui = {
--          -- Maximum number of items to show in the menu picker.
--          -- If the list exceeds this number, a different picker (like fzf) is used.
--          menu_max = 15, -- Recommended: 10–20. Max: 36.

--          -- Picker strategy:
--          -- "auto": uses menu if items <= menu_max, otherwise fzf (if available) or a filterable list
--          -- "fzf": always use fzf if available, otherwise fallback to a filterable list
--          picker = "auto", -- "auto" | "fzf"
--       },
--                       })


require("starship"):setup()


require("git"):setup()


-- Need additions, cannot deal with '-' in file ext like 'nsys-rep'!
-- require("mime-ext"):setup {
--    -- Expand the existing filename database (lowercase), for example:
--    with_files = {
--       makefile = "text/makefile",
--       Makefile = "text/makefile",
--     },
--    -- Expand the existing extension database (lowercase), for example:
--    with_exts = {
--       org = "text/org",
--       py = "text/python",
--       m = "text/matlab",
--       ipynb = "text/jupyter",
--       nsys-rep = "application/nsys",
--       f90 = "text/fortran",
--       f77 = "text/fortran",
--       f = "text/fortran",
--       gp = "text/gnuplot",
--    },
--    -- If the mime-type is not in both filename and extension databases,
--    -- then fallback to Yazi's preset `mime` plugin, which uses `file(1)`
--    fallback_file1 = true,
--                           }
-- Change rules in section open of yazi.toml:
-- { mime = "text/python", use = [ "python", "black" ] },
-- { mime = "text/matlab", use = "octave" },
-- { mime = "text/fortran", use = "fortran" },
-- { mime = "text/gnuplot", use = "gnuplot" },
-- { mime = "text/markdown", use = "pandoc" },
-- { mime = "text/org", use = "pandoc" },
-- { mime = "text/jupyter", use = "jupyter" },
-- { name = "*.nsys-rep", use = "nsys" },
-- { mime = "text/makefile", use = "make" },


-- Show user/group of files in status bar:
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


-- Custom shell.
-- require("custom-shell"):setup({
--     history_path = "default",
--     save_history = true,
--                              })


-- fr customisations like `--preview-window 'right:50%'`, does not work, that
-- has to be changed in the plugin `main.lua`.
-- require("fr"):setup
-- {
--    fzf = {
--       --"--layout=reverse",
--    },
--    -- rg moved to ~/.config/ripgreprc.
--    -- bat moved to ~/.config/bat/config.
--    -- moved to ~/.config/ripgrep-all
--    -- not sure if `rga_preview` does anything.
--    rga_preview = {
--       "--colors=line:fg:red",
--       "--colors=match:fg:magenta",
--       "--colors=match:style:nobold",
--    },
-- }
