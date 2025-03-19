{config, lib, pkgs, ...}:

{
  imports = [
    # Import the DigitalOcean module
    <nixpkgs/nixos/modules/virtualisation/digital-ocean.nix>
  ];

  # DigitalOcean specific configuration
  digitalOcean = {
    # Your DigitalOcean API token
    apiToken = "dop_v1_4718ee0e380f196caeeae2656e8b1947a1b519be3873f249b7d033e541483a83";


    # Droplet configuration
    droplet = {
      name = "my-nixos-droplet";
      region = "nyc3"; # Example: New York 3
      size = "s-1vcpu-1gb"; # Example: 1GB RAM, 1 vCPU
      image = "nixos-24.11"; # Or your desired NixOS version
      sshKeys = [ "" ]; # Your SSH key fingerprint
    };

    # Optionally, configure a floating IP
    # floatingIp = {
    #   region = "nyc3";
    # };
  };

  # Standard NixOS configuration
  networking.hostName = "my-nixos-droplet";
  networking.networkmanager.enable = true; # or your preferred networking solution.
  # other configuration...
  users.users.nixos = {
    isNormalUser = true;
    extraGroups = [ "wheel" ];
    openssh.authorizedKeys.keys = [
      # Your public ssh key goes here.
      "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIK75QoZORCkY2fyBX/vtbniOiCY11jgV7N/IyivqBn3o linux@linux-ThinkPad-P15v-Gen-1"
    ];
  };

  system.stateVersion = "24.11"; # Or your desired NixOS version

  # Optionally, enable SSH password authentication (not recommended for production)
  # services.openssh.enable = true;
  # services.openssh.passwordAuthentication = true;

  # Optionally, configure a firewall
  networking.firewall.allowedTCPPorts = [ 22 80 443 ]; # Example: Allow SSH, HTTP, HTTPS

  # Example: Install some packages
  environment.systemPackages = with pkgs; [
    vim
    git
    curl
    morph
  ];

  # Example: Enable the Nix daemon
  services.nix-daemon.enable = true;
}
