export ZSH=/home/halapenio/.oh-my-zsh

ZSH_THEME="clean"

DISABLE_UNTRACKED_FILES_DIRTY="true"
ENABLE_CORRECTION="true"

plugins=(git git-extras git-remote-branch nmap adb cp lein autojump python)

source $ZSH/oh-my-zsh.sh

export PATH=$HOME/.local/bin:$HOME/bin:/usr/bin/core_perl:/usr/bin/site_perl:/usr/bin/vendor_perl:$HOME/.pear/bin:/usr/local/bin:/usr/bin:/bin:/usr/sbin:/sbin

export LC_ALL="en_GB.UTF-8"
export VISUAL=emacsclient
export EDITOR=emacsclient
export SUDO_EDITOR=emacsclient
export ALTERNATE_EDITOR=vim

export PARINIT="rTbgqR B=.,?_A_a Q=_s>|"

export QEMU_AUDIO_DRV=alsa
export QEMU_ALSA_DAC_BUFFER_SIZE=512
export QEMU_ALSA_DAC_PERIOD_SIZE=170
export GTAGSFORCECPP=true
export MOZ_USE_OMTP=1
export MOZ_ACCELERATED=1
export MOZ_WEBRENDER=1

export DISPLAY=:0

export BROWSER=/usr/bin/iceweasel

export XDG_RUNTIME_DIR=/run/user/1000
export _JAVA_AWT_WM_NONREPARENTING=1
export _JAVA_OPTIONS='-Dawt.useSystemAAFontSettings=on -Dswing.aatext=true'
export LEIN_FAST_TRAMPOLINE=1

ulimit -c unlimited

alias gdb='gdb -q'
alias bc='bc -l -q'
alias top='top -U `whoami`'
alias du='du -s'
alias df='df -h'
alias scp='scp -q'
alias feh='feh -F -r -x --auto-rotate'
alias xz='xz -T`nproc`'
alias gpg='torify gpg'
alias less='less -FX'
alias diff='diff --color=always'
alias idris='idris --nobanner'

function ls () {
  /usr/bin/ls "$@" --color | less -FX
}

source /usr/share/zsh/plugins/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh

# for Emacs and tramp
if [[ "$TERM" == "dumb" ]]
then
  unsetopt zle
  unsetopt prompt_cr
  unsetopt prompt_subst
  PS1='$ '
fi

# for tmux
[[ $- != *i* ]] && return
[[ -z "$TMUX" ]] && exec tmux
