# Use the official Haskell image to create a build artifact.
# https://hub.docker.com/_/haskell/
FROM haskell:8.6.5 as builder

# Copy local code to the container image.
WORKDIR /app
COPY . .

# Build
# Save time and space using system provided GHC, otherwise stack would install
# the compiler version specified in stack.yaml
RUN stack --system-ghc setup
RUN stack --system-ghc build

# Service must listen to $PORT environment variable.
# This default value facilitates local development.
ENV PORT 8080

# Run the web service on container startup.
CMD ["stack", "--system-ghc", "run"]