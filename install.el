
(defvar *pip-install-command*
  "pip3 install"
  "PIP3 command to install a dependency.")

(defvar *pip-dependencies*
  '("'python-language-server[all]'")
  "Pip packages for python based dependencies.")

(defvar *npm-install-command*
  "npm i -g"
  "NPM command to install a dependency.")

(defvar *npm-dependencies*
  '("bash-language-server"
    "typescript"
    "typescript-language-server"
    "vscode-css-languageserver-bin"
    "vscode-html-languageserver-bin"
    "vue-language-server"
    "javascript-typescript-stdio")
  "NPM packages for node based dependencies.")

(defvar *test-dependencies*
  '("bash-language-server"))

(defvar *golang-install-command*
  "go get -u"
  "Go command to install a dependency.")

(defvar *golang-dependencies*
  '("github.com/saibing/bingo"
    "")
  "Go based dependencies.")

(defvar *macOS-install-command*
  "brew install"
  "macOS command to install a dependency.")

(defvar *macOS-dependencies*
  '("cquery")
  "macOS specific dependencies.")

(defvar *linux-install-command*
  "sudo apt install"
  "Linux (default Ubuntu/Debian) comamnd to install a dependency.")

(defvar *linux-dependencies*
  '()
  "Linux specific dependencies.")

(defun install-npm-deps ()
  "Install all NPM based deps."
  (mapcar (lambda (x) (shell-command (concat *npm-install-command* " " x))) *npm-dependencies*))

(defun install-go-deps ()
  "Install all Go lang based deps."
  (mapcar (lambda (x) (shell-command (concat *golang-install-command* " " x))) *golang-dependencies*))
