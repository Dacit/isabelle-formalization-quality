# Isabelle Graph Importer
Isabelle formal entity graph importer

## Installation:

`./mvnw clean install`
`isabelle components -u <DIR>`

## Usage:
Start graph db (e.g., with `docker-compose up -d`), then run via `isabelle graph_import -?`.
When using the docker-compose, GRAPHDB is `localhost:7687`.
