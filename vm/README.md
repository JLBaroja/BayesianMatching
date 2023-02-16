# Using the VirtualBox computational environment

## First use

Assuming a Linux system

1. `sudo apt install virtualbox vagrant virtualbox-ext-pack virtualbox-guest-utils git`
2. `git clone git@github.com:JLBaroja/BayesianMatching.git`
3. `cd BayesianMatching/vm && vagrant up`
4. Point browser to `https://localhost:21187/`

In your BIOS, you may need to turn off secure boot and allow Virtualization Technology (VT), which may be disabled by default.

Vagrant and VirtualBox exist for most operating systems, including MS Windows.


## For developers

Before making the virtual machine, make directory `vm/secrets/.ssh/` and place GitHub private key in there as well as config file:

```
Host github.com
  User git
  Hostname github.com
  IdentityFile ~/.ssh/<keyfilename>
```
