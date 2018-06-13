# Hello World Ejabberd Module

This project demonstrates common techniques used in a custom ejabberd module:

- Subscribing to/ handling events:
  - Subscribe to friend requests (presence subscriptions)
- Call other modules to retrieve information about user
  - Retrieve user's friends list
- Pass configuration parameters into module  
  
  
## Requirements

- Ejabberd (tested with v18.01)
- Erlang


## Installation

### Directory
Create a sym link to this project in your `$HOME/.ejabberd-modules/sources` folder  

**Important files**

Ejabberd requires the following files:
 - a `.spec` file in the module folder root
 - a `README.txt` file (YES, i'm serious)
 - a `COPYING` file ... *sigh*

### Module Check (optional)

Run a module check to have the module be checked by ejabberd for validity

```bash
$ ejabberdctl module_check mod_helloworld
```

### Install module

You must first `install` a module to ejabberd before it can be used

```bash
$ ejabberdctl module_install mod_helloworld
```

**Note**:
- You must run a `module_uninstall` and then `module_install` if you make changes to the code
- You will receive errors if you try including the config of the module in ejabberd.yml before installing it


### Update ejabberd.yml 

This module demonstrates the passing of config params into a module.

Regardless of whether your module takes config params or not, you will need
to update the `ejabberd.yml` to include your module

The ejabberd config can be found in `$EJABBER_HOME/etc/ejabberd/ejabberd.yml`

See the `conf/mod_helloworld.yml.example` for the structure of the config.

Edit the `modules` section of the `ejabberd.yml`

```yaml
modules:
  mod_helloworld:
    url: "http://test.com"
```


