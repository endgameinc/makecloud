Homepage/Docs: [makecloud.dev](https://makecloud.dev)

Issues: Github Issues

This is a beta pre 1.0 zero release, lots of things are still being built, are
inprogress or may have bugs.

Makecloud is an innovative open source build & deploy tool that is designed to
work with large complex source repositories and modern software development
environments that deploy to the cloud. The first and most significant design
choice is to have Makecloud operate over a directed acyclic graph instead of
pipeline or linear set of stages. Makecloud takes advantage of a project’s
dependency graph to cache results from stages whose dependencies have not
changed since the last invocation, and to parallelize build stages when
dependencies allow.  This serves a couple purposes, it significantly speeds up
builds and it makes possible to include base image generation in your normal
build pipeline and only update them when needed. Writing and testing single
stages is a joy due to the speed and immediate feedback loop due to caching.
Every stage in Makecloud is run on a separate virtual machine that has been
spun up for just that purpose. This ensures that builds are not contaminated
due to changes and allows the system to scale to execute as many stages in
parallel as you can get compute resources for.

In addition, there are several features that make Makecloud easy to build
deploy graphs with. There is built-in support for authenticated storage and
retrieval of files between stages so that you can focus on the important logic
rather than plumbing. Configuration files are also simple and straightforward
to use; most people can understand and write one with only a minute of
explanation. Makecloud also comes as a command line tool that can be invoked
locally without any other requirements. This makes it easy to tie into legacy
systems as a method of running your new CI/CD.

