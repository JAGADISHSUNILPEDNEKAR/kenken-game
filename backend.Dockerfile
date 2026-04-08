# Stage 1: Build Environment
FROM haskell:9.4.8 AS builder
WORKDIR /app

# Copy the stack project configurations
COPY stack.yaml stack.yaml.lock package.yaml ./

# Pre-build dependencies to utilize Docker layer caching
RUN stack build --test --only-dependencies

# Copy the rest of the project source
COPY . .

# Build and install the binaries to a known path
RUN stack install --local-bin-path /opt/build/bin

# Stage 2: Minimal Runtime Environment
FROM debian:bullseye-slim
WORKDIR /app

# Install standard C library requirements for Haskell binaries
RUN apt-get update && apt-get install -y \
    libgmp10 \
    libnuma1 \
    && rm -rf /var/lib/apt/lists/*

# Copy the compiled executable from the builder stage
COPY --from=builder /opt/build/bin/kenken-server /usr/local/bin/kenken-server

# Expose the correct backend port
EXPOSE 3001

# Start the server
CMD ["kenken-server"]
