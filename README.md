# Examples Using the [OpenFaaS R Templates](https://github.com/analythium/openfaas-rstats-templates)

This folder contains the following worked examples:

- [Hello World](00-hello/README.md)
- [Principal Component Analysis](01-principal-components/README.md)
- [Time Series Forecast](02-time-series-forecast/README.md)
- [Classification](03-classification/README.md)

More examples are on the way.
Please submit a PR if you have an interesting use case that
you'd like to see here.

## Prerequisites

These are prerequisites for all the examples.

__Step 1.__ Install the [OpenFaaS CLI](https://docs.openfaas.com/cli/install/).

__Step 2.__ Set up your [k8s, k3s, or faasd with OpenFaaS](https://docs.openfaas.com/deployment/).

It is recommended to read the [OpenFaaS docs](https://docs.openfaas.com/) first
and set up a local or remote [Kubernetes cluster](https://docs.openfaas.com/deployment/kubernetes/) or [faasd](https://docs.openfaas.com/deployment/faasd/) with
OpenFaaS deployed. To get going quickly,
follow the official OpenFaaS [workshop](https://docs.openfaas.com/tutorials/workshop/), or enroll into the free
[Introduction to Serverless on Kubernetes](https://www.edx.org/course/introduction-to-serverless-on-kubernetes) course.

> If you are looking for the smallest footprint (single node), the book [Serverless For Everyone Else](https://gumroad.com/a/494294131/fKOcw) by Alex Ellis is highly recommended.

__Step 3.__ Use `docker login` to log into your registry of choice for pushing images.
Export your Docker Hub user or organization name:

```bash
export OPENFAAS_PREFIX="" # Populate with your Docker Hub username
```

__Step 4.__ Log into your OpenFaaS instance (see more info [here](https://github.com/openfaas/workshop/blob/master/lab1b.md)):

```bash
export OPENFAAS_URL="http://127.0.0.1:8080" # Populate with your OpenFaaS URL

# This command retrieves your password
PASSWORD=$(kubectl get secret -n openfaas basic-auth -o jsonpath="{.data.basic-auth-password}" | base64 --decode; echo)

# This command logs in and saves a file to ~/.openfaas/config.yml
echo -n $PASSWORD | faas-cli login --username admin --password-stdin
```

Note: use `http://127.0.0.1:8080` as your OpenFaaS URL when using port forwarding via:

```bash
kubectl port-forward svc/gateway -n openfaas 8080:8080
```

__Step 5.__ Use the [`faas-cli`](https://github.com/openfaas/faas-cli) and pull R templates:

```bash
faas-cli template pull https://github.com/analythium/openfaas-rstats-templates
```

Now you can pick one of the examples and continue.
