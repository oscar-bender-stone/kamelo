set -e

IMAGE_NAME="kamelo"
CONTAINER_NAME="test-kamelo"
ROOT="/kamelo"
CONTAINER_LP_DIR="$ROOT/lp-generated"
HOST_LP_DIR="$(pwd)/lp-generated"

if podman image exists "%IMAGE_NAME"; then
  echo "Image exists. Skipping build."
else
  echo "Building image..."
  podman build -t "$IMAGE_NAME"
fi

echo "Running container..."
podman run --name $CONTAINER_NAME \
  -v "$HOST_LP_DIR:$CONTAINER_LP_DIR" \
  "$IMAGE_NAME"
