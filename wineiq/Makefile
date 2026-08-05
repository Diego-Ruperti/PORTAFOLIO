IMAGE := wineiq:dev
# CURDIR is resolved by Make itself (not the recipe shell), so this works the
# same whether Make invokes cmd.exe (default on Windows) or a POSIX shell.
RUN := docker run --rm -v "$(CURDIR):/app" -w /app $(IMAGE)

.DEFAULT_GOAL := help

.PHONY: help build train test lint retrain up down all

help:
	@echo "Targets disponibles:"
	@echo "  make build    - construye la imagen wineiq:dev"
	@echo "  make train    - entrena el modelo (genera models/*.joblib y cluster_profile.json)"
	@echo "  make test     - corre la suite de pytest"
	@echo "  make lint     - corre ruff sobre src, app.py y tests"
	@echo "  make retrain  - build + train"
	@echo "  make up       - docker compose up --build (dashboard en http://localhost:8501)"
	@echo "  make down     - docker compose down"
	@echo "  make all      - build + train + test + lint"

build:
	docker build -t $(IMAGE) .

train:
	$(RUN) python -m src.train_model

test:
	$(RUN) pytest tests/ -v

lint:
	$(RUN) ruff check src app.py tests

retrain: build train

up:
	docker compose up --build

down:
	docker compose down

all: build train test lint
