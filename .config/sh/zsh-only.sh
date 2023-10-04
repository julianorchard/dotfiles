tsssh() {
  # Use FZF to search for Tailscale IP's to SSH to
  [ ! -x /usr/bin/tailscale ] && return 1
  # Get output of only 'online' machines with Tailscale
  online_list=$(sudo tailscale status | grep -v offline | grep -v \#)
  # Get the friendly name of the machines with FZF
  machine_name=$(echo $online_list | awk '{ print $2 }' | fzf)
  # Get the IP of the machine chosen with FZF
  machine_ip=$(echo $online_list | grep ${machine_name} | awk '{ print $1 }')
  # Prepare to SSH to the chosen machine (weird zsh syntax)
  #read "machine_username?Username for ${machine_name}: "
  ssh cw-devops@${machine_ip} -i ~/.ssh/cw-group-key
}
