set fish_user_paths /usr/lib/ccache/bin $HOME/bin /usr/bin/core_perl /usr/bin/site_perl /usr/bin/vendor_perl $HOME/.pear/bin

set GTAGSFORCECPP true

set LC_ALL "en_GB.UTF-8"
set VISUAL emacsclient
set EDITOR emacsclient
set SUDO_EDITOR emacsclient
set ALTERNATE_EDITOR vim

set PARINIT "rTbgqR B=.,?_A_a Q=_s>|"

set QEMU_AUDIO_DRV alsa
set QEMU_ALSA_DAC_BUFFER_SIZE 512
set QEMU_ALSA_DAC_PERIOD_SIZE 170
set GTAGSFORCECPP true
set MOZ_USE_OMTP 1
set MOZ_ACCELERATED 1
set MOZ_WEBRENDER 1

set BROWSER /usr/bin/firefox

set _JAVA_OPTIONS '-Dawt.useSystemAAFontSettings=on -Dswing.aatext=true -Dswing.defaultlaf=com.sun.java.swing.plaf.gtk.GTKLookAndFeel -Dswing.crossplatformlaf=com.sun.java.swing.plaf.gtk.GTKLookAndFeel -Dsun.java2d.opengl=true'
set _JAVA_AWT_WM_NONREPARENTING 1

# set GDK_BACKEND wayland
# set SDL_VIDEODRIVER wayland
# set CLUTTER_BACKEND wayland
# set QT_QPA_PLATFORM wayland-egl
# set QT_DISABLE_WINDOWDECORATION 1
# set QT_WAYLAND_DISABLE_WINDOWDECORATION 1
# set ECORE_EVAS_ENGINE wayland_egl
# set ELM_ENGINE wayland_egl
# set XDG_SESSION_TYPE wayland
set XKB_DEFAULT_LAYOUT us,ru
set XKB_DEFAULT_VARIANT ,nodeadkeys
set XKB_DEFAULT_OPTIONS grp:shifts_toggle,

ulimit -c unlimited

alias gdb='gdb -q'
alias bc='bc -l -q'
alias top='top -U $USER'
alias du='du -s'
alias df='df -h'
alias scp='scp -q'
alias feh='feh -F -r -x --auto-rotate'
alias xz='xz -T`nproc`'
alias gpg='torify gpg'
alias less='less -FX'
alias diff='diff --color=always'
alias idris='idris --nobanner'

function ls --description 'List contents of directory with `less` command'
  command ls --color $argv | less -rFX
end

function fish_prompt
  set_color yellow
  echo '$ '
end

test $TERM != 'screen' ; and test $TERM != 'dumb' ; and exec tmux

if test "$TERM" = "dumb"
  function fish_title; end
end

fish_vi_key_bindings
