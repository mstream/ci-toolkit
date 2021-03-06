### Description 

The intention of this suite of commands is to keep the track 
the progress of CI pipelines inside a Git repository 
instead of CI servers. 
In fact, this solution eliminates the necessity of using stateful CI 
servers at all as each steps of pipelines can be executed by independent,
continusously running processes. 

### How to...

#### set up a workflow consisting of three CI stages

Process A:
```shell
# cloning the repository 
git clone git@github.com:mstream/ci-toolkit.git

# running unit tests
./run-unit-tests.sh

# marking the commit
npx @ci-toolkit/pipeline mark-commit --ci-stage unit-testing --commit-ref $(git rev-parse HEAD)

# push notes
git push origin refs/notes/*
```

Process B:
```shell
# cloning the repository 
git clone git@github.com:mstream/ci-toolkit.git

# checking out the last commit which passed unit tests
git checkout $(npx @ci-toolkit/pipeline get-last --ci-stage unit-testing)

# running functional tests
./run-functional-tests.sh

# marking the commit
npx @ci-toolkit/pipeline mark-commit --ci-stage functional-testing --commit-ref $(git rev-parse HEAD)

# push notes
git push origin refs/notes/*
```

Process C:
```shell
# cloning the repository 
git clone git@github.com:mstream/ci-toolkit.git

# checking out the last commit which passed both unit tests and functional tests
git checkout $(npx @ci-toolkit/pipeline get-last --ci-stage unit-testing --ci-stage functional-testing)

# running functional tests
./run-integration-tests.sh

# marking the commit
npx @ci-toolkit/pipeline mark-commit --ci-stage integration-testing --commit-ref $(git rev-parse HEAD)

# push notes
git push origin refs/notes/*
```

### Commands

- [get-last](docs/get-last.md)
- [mark-commit](docs/mark-commit.md)
