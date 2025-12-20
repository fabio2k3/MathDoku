#!/usr/bin/env bash
set -e

# Colores para que quede bonito
GREEN='\033[0;32m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

echo -e "${BLUE}== MathDoku – Commit asistido ==${NC}"

echo "Tipos disponibles:"
echo "  feat  - nueva funcionalidad"
echo "  fix   - corrección de error"
echo "  docs  - documentación"
echo "  chore - tareas de mantenimiento/refactor"
echo "  test  - pruebas"
echo "  init  - inicialización del proyecto"

read -p "Tipo de commit (feat/fix/docs/chore/test/init): " type
read -p "Resumen breve (imperativo, sin punto final): " summary
read -p "Número de issue (solo número, ej: 1): " issue

# Sanitizar
type="$(echo "$type" | tr '[:upper:]' '[:lower:]' | xargs)"
summary="$(echo "$summary" | xargs)"
issue="$(echo "$issue" | xargs)"

if [[ -z "$type" || -z "$summary" || -z "$issue" ]]; then
    echo "Error: tipo, resumen e issue son obligatorios."
    exit 1
fi

case "$type" in
    feat|fix|docs|chore|test|init)
        ;;
    *)
        echo "Error: tipo '$type' no válido."
        exit 1
        ;;
esac

message="$type: $summary (#$issue)"

echo
echo -e "Mensaje de commit generado:"
echo -e "${GREEN}  $message${NC}"
echo

read -p "¿Confirmar y ejecutar 'git commit'? [s/N]: " confirm

if [[ "$confirm" == "s" || "$confirm" == "S" ]]; then
    git add .
    git commit -m "$message"
    echo -e "${GREEN}Commit realizado con éxito.${NC}"
else
    echo "Commit cancelado."
fi