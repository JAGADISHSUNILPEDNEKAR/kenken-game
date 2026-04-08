# frontend.Dockerfile
FROM node:18-alpine AS builder
WORKDIR /app

# Install dependencies
COPY frontend/package.json ./
RUN npm install

# Build the Astro site
COPY frontend/ ./
RUN npm run build

# Stage 2: Serve static files with Nginx
FROM nginx:alpine
COPY frontend/nginx.conf /etc/nginx/conf.d/default.conf
COPY --from=builder /app/dist /usr/share/nginx/html

EXPOSE 80
CMD ["nginx", "-g", "daemon off;"]
