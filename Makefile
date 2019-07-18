# make VERBOSE nonempty to see raw commands (or provide on command line)
ifndef VERBOSE
VERBOSE:=
endif

# use SHOW to inform user of commands
SHOW:=@echo

# use HIDE to run commands invisibly, unless VERBOSE defined
HIDE:=$(if $(VERBOSE),,@)

# Invoking `NO_CACHE=1 make docker-build` will cause containers to be built
# from scratch (as we do in CI) but will take considerably longer to complete;
# by default we skip this option for local development as a convenience
ifdef NO_CACHE
NO_CACHE="--no-cache"
endif

.PHONY: all rps
all: rps

rps:
	cd examples/rps-auto && $(MAKE) all


# Docker part
DOCKER_COMPOSE = docker-compose
DOCKER_COMPOSE_FILE = docker/docker-compose.yml

docker-pull: ## Pull Alacris prerequisites images
	$(SHOW) " Pulling Alacris Docker images"
	$(HIDE) docker/scripts/pull_images.sh

docker-build: ## Build all or c=<name> containers in foreground
	@$(DOCKER_COMPOSE) -f $(DOCKER_COMPOSE_FILE) build $(NO_CACHE) $(c)

docker-list: ## List available services
	@$(DOCKER_COMPOSE) -f $(DOCKER_COMPOSE_FILE) config --services

docker-up: ## Start all or c=<name> containers in foreground
	@$(DOCKER_COMPOSE) -f $(DOCKER_COMPOSE_FILE) up $(c)

docker-start: ## Start all or c=<name> containers in background
	@$(DOCKER_COMPOSE) -f $(DOCKER_COMPOSE_FILE) up -d $(c)

docker-stop: ## Stop all or c=<name> containers
	@$(DOCKER_COMPOSE) -f $(DOCKER_COMPOSE_FILE) stop $(c)

docker-restart: ## Restart all or c=<name> containers
	@$(DOCKER_COMPOSE) -f $(DOCKER_COMPOSE_FILE) stop $(c)
	@$(DOCKER_COMPOSE) -f $(DOCKER_COMPOSE_FILE) up -d $(c)

docker-status: ## Show status of containers
	@$(DOCKER_COMPOSE) -f $(DOCKER_COMPOSE_FILE) ps

docker-clean:  ## Clean all data
	@$(DOCKER_COMPOSE) -f $(DOCKER_COMPOSE_FILE) down -v

docker-recompile: ## Recompile application
	$(SHOW) "Recompiling Alacris apps"
	@$ docker/scripts/recompile.sh

docker-prune: ## Delete dangling images
	$(SHOW) Deleting dangling docker images
	$(HIDE) docker system prune