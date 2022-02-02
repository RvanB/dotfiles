#!/bin/bash

# Terminal setup script
# Raiden van Bronkhorst

GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m'

# Zsh
printf "${GREEN}Installing zsh...${NC}\n"
FILE=/usr/bin/zsh
sudo apt install zsh
printf "${YELLOW}Changing login shell...${NC}\n"
chsh -s /usr/bin/zsh

# Kitty
printf "${GREEN}Installing kitty...${NC}\n"
curl -L https://sw.kovidgoyal.net/kitty/installer.sh | sh /dev/stdin

printf "${YELLOW}Linking kitty binary to directory on PATH...${NC}\n"
sudo ln -s ~/.local/kitty.app/bin/kitty /usr/local/bin/kitty 2> /dev/null

printf "${YELLOW}Adding kitty to desktop database...${NC}\n"
mkdir -p ~/.local/share/applications
cp ~/.local/kitty.app/share/applications/kitty.desktop ~/.local/share/applications/
sed -i "s|Icon=kitty|Icon=/home/$USER/.local/kitty.app/share/icons/hicolor/256x256/apps/kitty.png|g" ~/.local/share/applications/kitty.desktop
update-desktop-database ~/.local/share/applications

printf "${GREEN}Installing Miniconda...${NC}\n"
wget https://repo.anaconda.com/miniconda/Miniconda3-py39_4.10.3-Linux-x86_64.sh -O /tmp/MinicondaInstaller.sh

chmod +x /tmp/MinicondaInstaller.sh
/tmp/MinicondaInstaller.sh

# Neovim
printf "${GREEN}Installing Neovim...${NC}\n"
sudo apt install neovim

printf "${YELLOW}Installing Vundle...${NC}\n"
git clone https://github.com/VundleVim/Vundle.vim.git ~/.vim/bundle/Vundle.vim

printf "${YELLOW}Installing plugins...${NC}\n"
nvim --headless +PluginInstall +qall

printf "$\n{GREEN}Linking dotfiles...${NC}\n"

printf "${YELLOW}~/.config/kitty/kitty.conf${NC}\n"
mkdir -p ~/.config/kitty/
ln -sn $PWD/kitty/kitty.conf ~/.config/kitty/kitty.conf 2> /dev/null
ln -sn $PWD/kitty/custom-dark.conf ~/.config/kitty/custom-dark.conf 2> /dev/null

printf "${YELLOW}~/.config/nvim/init.vim${NC}\n"
printf "${YELLOW}~/.config/nvim/colors/inkpot.vim${NC}\n"
mkdir -p ~/.config/nvim/colors
ln -sn $PWD/nvim/init.vim ~/.config/nvim/init.vim 2> /dev/null
ln -sn $PWD/nvim/colors/inkpot.vim ~/.config/nvim/colors/inkpot.vim 2> /dev/null

printf "${YELLOW}~/.tmux.conf${NC}\n"
ln -sn $PWD/tmux/tmux.conf ~/.tmux.conf 2> /dev/null

printf "${YELLOW}~/.zshrc${NC}\n"
ln -sn $PWD/zsh/zshrc ~/.zshrc 2> /dev/null

