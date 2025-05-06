local shell = os.getenv("SHELL"):match(".*/(.*)")

local preview_opts = {
   default = [===[line={2} && begin=$( if [[ $line -lt 7 ]]; then echo $((line-1)); else echo 6; fi ) && bat --number --highlight-line={2} --color=always --line-range $((line-begin)):$((line+45)) {1}]===],
}
local preview_cmd = preview_opts[shell] or preview_opts.default

local rg_prefix = "rg --hidden --column --line-number --no-heading --color=always --smart-case "

local fzf_args = [[fzf --color=dark --no-info --no-separator --color='prompt:magenta' --inline-info --border=none --preview-window=border-none --layout=reverse --ansi --preview='bat --number --color=always {1}' ]]
local rg_args = {
   default = [[fzf --color=dark --no-info --no-separator --color='prompt:magenta' --inline-info --border=none --preview-window=border-none --layout=reverse --ansi --disabled  --bind "start:reload:]]
      .. rg_prefix
      .. [[{q}" --bind "change:reload:sleep 0.1; ]]
      .. rg_prefix
      .. [[{q}" || true" --delimiter : --preview --preview-window "right:50%"']]
      .. preview_cmd
      .. [[' --nth '3..']],
}
local fg_args = [[rg --color=always --line-number --no-heading --smart-case '' | fzf --ansi --preview=']]
   .. preview_cmd
   .. [[' --delimiter=':' --preview-window='right' --nth='3..']]

local function split_and_get_first(input, sep)
   if sep == nil then
      sep = "%s"
   end
   local start, _ = string.find(input, sep)
   if start then
      return string.sub(input, 1, start - 1)
   end
   return input
end

local state = ya.sync(function() return cx.active.current.cwd end)

local function fail(s, ...) ya.notify { title = "fg", content = string.format(s, ...), timeout = 5, level = "error" } end

local function entry(_, args)
   local _permit = ya.hide()
   local cwd = tostring(state())
   local cmd_args = ""

   if args[1] == "fzf" then
      cmd_args = fzf_args
   elseif args[1] == "rg" then
      cmd_args = rg_args[shell] or rg_args.default
   else
      cmd_args = fg_args
   end

   local child, err = Command(shell)
      :args({ "-c", cmd_args })
      :cwd(cwd)
      :stdin(Command.INHERIT)
      :stdout(Command.PIPED)
      :stderr(Command.INHERIT)
      :spawn()

   if not child then
      return fail("Spawn command failed with error code %s.", err)
   end

   local output, err = child:wait_with_output()
   if not output then
      return fail("Cannot read `fzf` output, error code %s", err)
   elseif not output.status.success and output.status.code ~= 130 then
      return fail("`fzf` exited with error code %s", output.status.code)
   end

   local target = output.stdout:gsub("\n$", "")

   local file_url = split_and_get_first(target, ":")

   if file_url ~= "" then
      ya.manager_emit(file_url:match("[/\\]$") and "cd" or "reveal", { file_url })
   end
end

return { entry = entry }
