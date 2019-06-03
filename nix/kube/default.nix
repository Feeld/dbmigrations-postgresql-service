/*
nix eval -f nix/kube --json result \
  --argstr tag 125d9e7ae771ac4244ec7d5f71a756fd1c977a03 \
  --arg replicas 1 \
  | kubectl apply -f -

*/
{ tag ? "latest"
, branch ? "master"
, name ? "fld-staging/dbm-service"
, namespace ? "devops"
, registry ? "gcr.io"
, apiPort ? 8080
, replicas ? 1
# Use to override the host.
, host ? null
}:
let
  inherit (pkgs)
    kubenix;

  lib = pkgs.lib;
  pkgs = import ../. {};


  isProduction = branch == "master";
  subdomain = if isProduction then "prd" else "stg";

  hosts = if host == null
    then

      if namespace == "default"
        then
          [ "dbm-service.${subdomain}.fld.services"
            "dbm-service.${subdomain}.fld.systems"
          ]
        else
          [ "dbm-service.${namespace}.${subdomain}.fld.services"
            "dbm-service.${namespace}.${subdomain}.fld.systems"
          ]

    else [ host ];

  defaultHost = builtins.elemAt hosts 0;

  serviceMetadata = {
    inherit namespace;
    labels =  {
      # Set of recommended labels
      # https://kubernetes.io/docs/concepts/overview/working-with-objects/common-labels/#labels
      "app.kubernetes.io/name" = "dbm-service";
      "app.kubernetes.io/instance" = "dbm-service";
      "app.kubernetes.io/component" = "http-service";
      "app.kubernetes.io/part-of" = "dbm-service";
    };
  };

in with pkgs.lib;
rec {
  # evaluated configuration
  config = (kubenix.evalModules {
    modules = [
      { docker.registry.url = registry;
        docker.images.dbm-service.image =
          import ./image.nix {
            inherit pkgs tag name;
            port = apiPort;
          };
        imports = with kubenix.modules; [ k8s docker ];

        kubernetes.api.secrets.environment = {
          metadata =  { inherit namespace; };
          type = "Opaque";
          data = {
            MIGRATION_ACCESS_TOKEN =
              kubenix.lib.toBase64 (builtins.readFile ./secrets/accessToken);
          };
        };

        kubernetes.api.namespaces."${namespace}" = {};

        kubernetes.api.deployments.dbm-service = {
          metadata = serviceMetadata;

          spec = {
            inherit replicas;
            selector.matchLabels.component = "dbm-service";
            template = {
              metadata.labels.component = "dbm-service";
              spec = {
                containers.dbm-service = {
                  image = config.docker.images.dbm-service.path;
                  imagePullPolicy = "IfNotPresent";
                  ports =
                    [ { containerPort = apiPort; }
                    ];
                  # TODO: add high and low requests settings, which could be
                  #   selected with an arg to the nix script.
                  resources = {
                    limits = {
                      cpu = "400m";
                      memory = "256Mi";
                    };
                    requests = {
                      cpu = "200m";
                      memory = "128Mi";
                    };
                  };
                  # TODO: uncomment once these end points are implemented.
                  # Used to determine if pod should be restarted.
                  # https://kubernetes.io/docs/tasks/configure-pod-container/configure-liveness-readiness-probes/#define-a-liveness-command
                  # livenessProbe = {
                  #   httpGet = {
                  #     path = "/dbm-service/liveness";
                  #     port = apiPort;
                  #   };
                  #   initialDelaySeconds = 5;
                  #   periodSeconds = 5;
                  #   timeoutSeconds = 5;
                  #   failureThreshold = 5;
                  # };
                  # Used to determine if pod is ready to receive traffic.
                  # https =//kubernetes.io/docs/tasks/configure-pod-container/configure-liveness-readiness-probes/#define-readiness-probes
                  # readinessProbe = {
                  #   httpGet = {
                  #     path = "/dbm-service/readiness";
                  #     port = apiPort;
                  #   };
                  #   initialDelaySeconds = 2;
                  #   periodSeconds = 3;
                  #   timeoutSeconds = 5;
                  #   failureThreshold = 5;
                  # };
                  envFrom =
                    [ { secretRef = { name = "environment"; }; }
                    ];
                };
              };
            };
          };
        };

        kubernetes.api.services.dbm-service = {
          metadata = serviceMetadata;
          spec = {
            ports = map ({port,name}: {
              inherit port name;
              protocol = "TCP";
              targetPort = port;
            }) [ {port=apiPort; name="http";} ];
            selector.component = "dbm-service";
          };
        };

        kubernetes.api.ingresses.dbm-service = {
          metadata = {
            inherit (serviceMetadata) namespace;
            annotations = serviceMetadata.labels // {
              "kubernetes.io/ingress.class" = "nginx";
              "kubernetes.io/ingress.allow-http" = "false";
              "certmanager.k8s.io/cluster-issuer" = "fld-letsencrypt-prd";
              "kubernetes.io/tls-acme" = "true";
              };
            };
          spec = {
            tls =
              [ { inherit hosts;
                  secretName = "tls-cert";
                }
              ];
            rules = map (host:
              { inherit host;
                http =
                  { paths =
                    [ { path = "/";
                        backend = {
                          serviceName = "dbm-service";
                          servicePort = apiPort;
                          };
                       }
                    ];
                  };
                }) hosts;
          };
        };
      }
    ];
  }).config;

  result = kubenix.lib.k8s.mkHashedList { items = config.kubernetes.objects; };

  environment = k8sConfig // k8sSecrets;

  image = config.docker.export;
}
