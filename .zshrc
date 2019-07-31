export ZSH=/home/halapenio/.oh-my-zsh

ZSH_THEME="clean"

DISABLE_UNTRACKED_FILES_DIRTY="true"
ENABLE_CORRECTION="true"

plugins=(git git-extras git-remote-branch nmap adb cp lein autojump python)

source $ZSH/oh-my-zsh.sh

export PATH=$HOME/bin:$HOME/.local/bin:$HOME/.cask/bin:$HOME/.luarocks/bin:$HOME/.gem/ruby/2.6.0/bin:$HOME/bin:$HOME/.cargo/bin:/usr/bin/core_perl:/usr/bin/site_perl:/usr/bin/vendor_perl:$HOME/.pear/bin:/usr/local/bin:/usr/bin:/bin:/usr/sbin:/sbin

export LC_ALL="en_GB.UTF-8"
export VISUAL=emacsclient
export EDITOR=emacsclient
export SUDO_EDITOR=emacsclient
export ALTERNATE_EDITOR=vim

export PARINIT="rTbgqR B=.,?_A_a Q=_s>|"

# export QEMU_AUDIO_DRV=alsa
# export QEMU_ALSA_DAC_BUFFER_SIZE=512
# export QEMU_ALSA_DAC_PERIOD_SIZE=170
export QEMU_AUDIO_DRV=pa
export GTAGSFORCECPP=true
export MOZ_USE_OMTP=1
export MOZ_ACCELERATED=1
export MOZ_WEBRENDER=1
# export MOZ_ENABLE_WAYLAND=1

export BROWSER=/usr/bin/firefox-developer-edition

export _JAVA_AWT_WM_NONREPARENTING=1
export _JAVA_OPTIONS='-Dawt.useSystemAAFontSettings=lcd -Dswing.aatext=true -Dswing.defaultlaf=com.sun.java.swing.plaf.gtk.GTKLookAndFeel -Dswing.crossplatformlaf=com.sun.java.swing.plaf.gtk.GTKLookAndFeel -Dsun.java2d.opengl=true -Djdk.gtk.version=3'
_SILENT_JAVA_OPTIONS="$_JAVA_OPTIONS"
unset _JAVA_OPTIONS
alias java='java "$_SILENT_JAVA_OPTIONS"'

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
alias sbcl='sbcl --noinform'

function ls () {
  /usr/bin/ls "$@" --color | less -FX
}

source /usr/share/zsh/plugins/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh

# export GDK_BACKEND=wayland
# export SDL_VIDEODRIVER=wayland
# export CLUTTER_BACKEND=wayland
# export ECORE_EVAS_ENGINE=wayland_egl
# export ELM_ENGINE=wayland_egl
# export QT_QPA_PLATFORM=wayland-egl
# export QT_DISABLE_WINDOWDECORATION=1
# export QT_WAYLAND_DISABLE_WINDOWDECORATION=1
# export XDG_SESSION_TYPE=wayland
# export XKB_DEFAULT_LAYOUT=us,ru
# export XKB_DEFAULT_OPTIONS=grp:caps_toggle

# for Emacs and tramp
if [[ "$TERM" == "dumb" ]]
then
  unsetopt zle
  unsetopt prompt_cr
  unsetopt prompt_subst
  PS1='$ '
fi

# start sway
# if [[ -z $DISPLAY ]] && [[ $(tty) = /dev/tty1 ]]; then
#   exec sway
# else # start tmux
#   [[ $- != *i* ]] && return
#   [[ -z "$TMUX" ]] && exec tmux
# fi

[[ $- != *i* ]] && return
[[ -z "$TMUX" ]] && exec tmux
