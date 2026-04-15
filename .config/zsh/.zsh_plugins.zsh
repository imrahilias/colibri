if ! (( $+functions[zsh-defer] )); then
  fpath+=( "$HOME/.cache/antidote/github.com/romkatv/zsh-defer" )
  source "$HOME/.cache/antidote/github.com/romkatv/zsh-defer/zsh-defer.plugin.zsh"
fi
fpath+=( "$HOME/.cache/antidote/github.com/mattmc3/ez-compinit" )
zsh-defer source "$HOME/.cache/antidote/github.com/mattmc3/ez-compinit/ez-compinit.plugin.zsh"
fpath+=( "$HOME/.cache/antidote/github.com/zsh-users/zsh-completions/src" )
zsh-defer source "$HOME/.cache/antidote/github.com/zsh-users/zsh-completions/src/src.plugin.zsh"
fpath+=( "$HOME/.cache/antidote/github.com/zsh-users/zsh-autosuggestions" )
zsh-defer source "$HOME/.cache/antidote/github.com/zsh-users/zsh-autosuggestions/zsh-autosuggestions.plugin.zsh"
fpath+=( "$HOME/.cache/antidote/github.com/zdharma-continuum/fast-syntax-highlighting" )
zsh-defer source "$HOME/.cache/antidote/github.com/zdharma-continuum/fast-syntax-highlighting/fast-syntax-highlighting.plugin.zsh"
