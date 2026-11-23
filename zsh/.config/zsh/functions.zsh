######## Functions ##########-----------------------------------------------------------
DISABLE_AUTO_TITLE="true"
function xtitle() {
	echo -en "\e]0;$@\a"
}
autoload -Uz add-zsh-hook
# updates the window title whenever a command is run
precmd_update_title() {
	xtitle "$(print -P $USER @ %3~)"
}
add-zsh-hook precmd precmd_update_title
######### venv utilities #########-----------------
# Activate the python virtual environment specified.
# If none specified, use 'venv'.
function vr() {
	local name="${1:-.venv}"
	local venvpath="${name:P}"

	# Check if 'venv' or '.venv' exists
	if [[ ! -d "${venvpath}" && ! -d "${venvpath}/.venv" ]]; then
		echo >&2 "Error: no such venv in current directory: ${name}"
		return 1
	fi

	# Set venvpath to the existing directory
	if [[ -d "${venvpath}/.venv" ]]; then
		venvpath="${venvpath}/.venv"
	fi

	if [[ ! -f "${venvpath}/bin/activate" ]]; then
		echo >&2 "Error: '${name}' is not a proper virtual environment"
		return 1
	fi

	. "${venvpath}/bin/activate" || return $?
	echo "Activated virtual environment ${name}"
}

# Create a new virtual environment, with default name 'venv'.
function mkvenv() {
	local name="${1:-.venv}"
	local venvpath="${name:P}"

	python3 -m venv "${name}" || return
	echo >&2 "Created venv in '${venvpath}'"
	vr "${name}"
}
##########################################################
# Handle $0 according to the standard:
# https://zdharma-continuum.github.io/Zsh-100-Commits-Club/Zsh-Plugin-Standard.html
0="${${ZERO:-${0:#$ZSH_ARGZERO}}:-${(%):-%N}}"
0="${${(M)0:#/*}:-$PWD/$0}"

eval '
  function acs(){
    (( $+commands[python3] )) || {
      echo "[error] No python executable detected"
      return
    }
    alias | python3 "'"${0:h}"'/cheatsheet.py" "$@"
  }
'
#############################################################
#
# mkcd is equivalent to takedir
function mkcd takedir() {
	mkdir -p $@ && cd ${@:$#}
}
#############################################################
function d() {
	if [[ -n $1 ]]; then
		dirs "$@"
	else
		dirs -v | head -n 15
	fi
}
compdef _dirs d
#############################################################
#####| JAVA FUNCTIONS | ###################
jrun() {
	if [[ $# -ne 1 ]]; then
		echo "Usage: jrun <JavaFile.java>"
		return 1
	fi

	local java_file="$1"
	if [[ ! -f "$java_file" ]]; then
		echo "File not found: $java_file"
		return 2
	fi

	# Extract directory and filename
	local src_dir=$(dirname "$java_file")
	local filename=$(basename "$java_file")
	local classname="${filename%.java}"

	# Extract package name from the Java file (if any)
	local package_name=$(grep -m1 '^package ' "$java_file" | sed -E 's/package[[:space:]]+([a-zA-Z0-9_.]+);/\1/')

	# Build the fully qualified class name
	local fqcn="$classname"
	if [[ -n "$package_name" ]]; then
		fqcn="$package_name.$classname"
	fi

	# Define build output directory
	local build_dir="./build"

	# Create build directory
	mkdir -p "$build_dir"

	# Compile Java file to build directory
	javac -d "$build_dir" "$java_file"
	if [[ $? -ne 0 ]]; then
		echo "Compilation failed."
		return 3
	fi

	# Run the Java class with correct classpath and fully qualified name
	java -cp "$build_dir" "$fqcn"
	local run_status=$?

	# Clean up .class files from build directory
	rm -rf "$build_dir"

	return $run_status
}
