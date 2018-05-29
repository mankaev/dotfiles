export PATH=/usr/lib/ccache/bin:$HOME/.local/bin:$HOME/bin:/usr/bin/core_perl:/usr/bin/site_perl:/usr/bin/vendor_perl:$HOME/.gem/ruby/2.3.0/bin:$HOME/.pear/bin:/usr/local/bin:/usr/bin:/bin:/usr/sbin:/sbin

export GTAGSFORCECPP=true

export MOZ_DATA_REPORTING=0
export MOZ_DEVICES=0
export MOZ_PAY=0
export MOZ_SERVICES_FXACCOUNTS=0
export MOZ_SERVICES_HEALTHREPORTER=0
export MOZ_SERVICES_METRICS=0
export MOZ_TELEMETRY_REPORTING=0
export MOZ_USE_OMTP=1
export MOZ_USE_XINPUT2=1
export MOZ_WEBRENDER=1

export QEMU_AUDIO_DRV=alsa
export QEMU_ALSA_DAC_BUFFER_SIZE=512
export QEMU_ALSA_DAC_PERIOD_SIZE=170

export _JAVA_AWT_WM_NONREPARENTING=1

#export GDK_BACKEND=wayland
#export SDL_VIDEODRIVER=wayland
#export CLUTTER_BACKEND=wayland
#export QT_QPA_PLATFORM=wayland-egl
export QT_DISABLE_WINDOWDECORATION=1
export QT_WAYLAND_DISABLE_WINDOWDECORATION=1
#export ECORE_EVAS_ENGINE=wayland_egl
#export ELM_ENGINE=wayland_egl
#export XDG_SESSION_TYPE=wayland
export XKB_DEFAULT_LAYOUT=us,ru
export XKB_DEFAULT_OPTIONS=grp:shifts_toggle,

export XDG_RUNTIME_DIR=/run/user/1000

xset -b
xhost si:localuser:root
setxkbmap -layout "us,ru" -option "" && setxkbmap -option grp:shifts_toggle
display -window root xc:'#2B2B2B'

termite &
firefox &
telegram-desktop &
emacs &
i3
