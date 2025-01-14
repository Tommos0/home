# If not running interactively, don't do anything
[[ $- != *i* ]] && return

# Source global definitions
if [ -f /etc/bashrc ]; then
	 . /etc/bashrc
fi

set_emacs_additions() {
    # Enable emacs Eat integration
    [ -n "$EAT_SHELL_INTEGRATION_DIR" ] && source "$EAT_SHELL_INTEGRATION_DIR/bash"

    if [[ $INSIDE_EMACS == *vterm* ]]; then
        vterm_printf() {
            if [ -n "$TMUX" ] && ([ "${TERM%%-*}" = "tmux" ] || [ "${TERM%%-*}" = "screen" ]); then
                # Tell tmux to pass the escape sequences through
                printf "\ePtmux;\e\e]%s\007\e\\" "$1"
            elif [ "${TERM%%-*}" = "screen" ]; then
                # GNU screen (screen, screen-256color, screen-256color-bce)
                printf "\eP\e]%s\007\e\\" "$1"
            else
                printf "\e]%s\e\\" "$1"
            fi
        }

        vterm_prompt_end(){
            vterm_printf "51;A$(whoami)@$(hostname):$(pwd)"
        }
        PS1=$PS1'\[$(vterm_prompt_end)\]'
    fi
}


set_prompt() {
    # set a fancy prompt (non-color, unless we know we "want" color)
    case "$TERM" in
        xterm-color|*-256color) color_prompt=yes;;
    esac

    if [ "$color_prompt" = yes ]; then
        PS1='${debian_chroot:+($debian_chroot)}\[\033[01;32m\]\u@\h\[\033[00m\]:\[\033[01;34m\]\w\[\033[00m\]\$ '
    else
        PS1='${debian_chroot:+($debian_chroot)}\u@\h:\w\$ '
    fi
    unset color_prompt force_color_prompt

    # If this is an xterm set the title to user@host:dir
    case "$TERM" in
        xterm*|rxvt*)
            PS1="\[\e]0;${debian_chroot:+($debian_chroot)}\u@\h: \w\a\]$PS1"
            ;;
        *)
            ;;
    esac

    unset use_color safe_term match_lhs sh
}


set_aliases() {
		alias ls="ls --color=auto"
    alias cp="cp -i"                          # confirm before overwriting something
    alias ln="ln -i"                          # confirm before overwriting something
    alias df='df -h'                          # human-readable sizes
    alias free='free -m'                      # show sizes in MB
    alias more=less
    # Actually use aliases
    shopt -s expand_aliases
}


set_history() {
    # Bash won't get SIGWINCH if another process is in the foreground.
    # Enable checkwinsize so that bash will check the terminal size when
    # it regains control.  #65623
    # http://cnswww.cns.cwru.edu/~chet/bash/FAQ (E11)
    shopt -s checkwinsize

    # Expand the history size
    export HISTFILESIZE=10000
    export HISTSIZE=500

    # Enable history appending instead of overwriting. 
    shopt -s histappend

    # Append to history right away
    PROMPT_COMMAND='history -a'

    # Allow ctrl-S for history navigation (with ctrl-R)
    stty -ixon
}

set_completion() {
    # Enable bash programmable completion features in interactive shells
    if [ -f /usr/share/bash-completion/bash_completion ]; then
    . /usr/share/bash-completion/bash_completion
    elif [ -f /etc/bash_completion ]; then
    . /etc/bash_completion
    fi

    # Only complete file paths after `sudo`
    complete -cf sudo
    # Ignore case on auto-completion
    # Note: bind used instead of sticking these in .inputrc
    if [[ $iatest > 0 ]]; then bind "set completion-ignore-case on"; fi

    # Show auto-completion list automatically, without double tab
    if [[ $iatest > 0 ]]; then bind "set show-all-if-ambiguous On"; fi
}

set_misc() {
    # This allows the `root` user on the local machine to access the X server
    xhost +local:root > /dev/null 2>&1

    # Change the window title of X terminals
    case ${TERM} in
    xterm*|rxvt*|Eterm*|aterm|kterm|gnome*|interix|konsole*)
        PROMPT_COMMAND='echo -ne "\033]0;${USER}@${HOSTNAME%%.*}:${PWD/#$HOME/\~}\007"'
        ;;
    screen*)
        PROMPT_COMMAND='echo -ne "\033_${USER}@${HOSTNAME%%.*}:${PWD/#$HOME/\~}\033\\"'
        ;;
    esac

    # NVM
    export NVM_DIR="$HOME/.nvm"
    [ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"  # This loads nvm
    [ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"  # This loads nvm bash_completion

		# GUIX
		if [ -n "$GUIX_ENVIRONMENT" ]; then
				if [[ $PS1 =~ (.*)"\\$" ]]; then
						PS1="${BASH_REMATCH[1]} [env]\\\$ "
				fi
		fi

		# VCPKG
		VCPKG_ROOT=~/git/vcpkg
		[ -d "$VCPKG_ROOT" ] && export PATH="$VCPKG_ROOT:$PATH"

		# PyEnv
		export PYENV_ROOT="$HOME/.pyenv"
		[ -d "$PYENV_ROOT/bin" ] && export PATH="$PYENV_ROOT/bin:$PATH" && eval "$(pyenv init -)"

		# DirEnv
		command -v direnv &> /dev/null && eval "$(direnv hook bash)"
}

set_prompt
set_aliases
set_history
set_completion
set_misc
set_emacs_additions
