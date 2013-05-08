
Installation (VirtualBox, shared folders)
==========================================

1. Install a fresh [Ubuntu 12.10 server image]
(http://releases.ubuntu.com/quantal/ubuntu-12.10-server-amd64.iso). To make
things easier, use the same username as you use on your host machine. During the
installation, you will be prompted to install the OpenSSH server; do so if you
wish to connect via SSH before running the Smogon installation script in
step 6. Otherwise, it will be installed for you automatically in step 6.

2. (Optional): If you have trouble connecting to the Internet from the VBox
guest, try `VBoxManage modifyvm <name of vm> --natdnshostresolver1 on` from
the command line.

3. Make sure you have the smogon-redux repo checked out locally. Then, go to
Devices > Shared Folders and add the repo's directory. Make sure the folder
name field is `smogon`.

4. Setup port forwarding so we can access the SSH server and web server from
outside the VM. Devices > Network Adapters, and map guest ports 22 and 80 host
ports (I use 2222 and 2280 myself). Now you can ssh into the box using `ssh
localhost -p 2222` and access the webserver via `localhost:2280`.

5. Take a snapshot with Machine > Take Snapshot. Now, if you ever want to
reinstall Smogon from scratch (or something goes wrong), you can revert back
to the clean install in an instant.

6. Install by `curl -sL http://is.gd/A1y6y6 | sudo bash -s virtualbox`

7. Reboot: `sudo reboot`
(Yes, this step is necessary as the setup script modifies `/etc/environment`)

nREPL
=====

The server will run an nREPL on port 1234. Only connections from the machine
hosting the nREPL will be accepted. To access it, you will need to use SSH
tunneling. To setup a background SSH tunnel, use

  `ssh -Nf -L <local port>:<destination>:<remote-port> <gateway>`
  
For our local VirtualBox instance, the gateway is localhost on port 2222, and we
want to connect to localhost:1234.

  `ssh -Nf -L 21234:localhost:1234 localhost -p 2222`

To tunnel to the production server, try

  `ssh -Nf -L 31234:localhost:1234 smogon.com`

