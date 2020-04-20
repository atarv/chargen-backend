# Use the official Haskell image to create a build artifact.
# https://hub.docker.com/_/haskell/
FROM haskell:8.6.5 as builder

# Copy local code to the container image.
WORKDIR /app
COPY . .

# Build executable
RUN stack setup
RUN stack build --copy-bins

# Use a Docker multi-stage build to create a lean production image.
# https://docs.docker.com/develop/develop-images/multistage-build/#use-multi-stage-builds
FROM fpco/haskell-scratch:integer-gmp

# Copy the executable from the builder stage to the production image.
WORKDIR /root/
COPY --from=builder /root/.local/bin/chargen-exe .

# Copy datafiles
COPY assets/chargen.db /data/

# Run the web service on container startup.
CMD ["./chargen-exe", "/data/chargen.db"]