server {
    listen          80 default_server;
    listen          [::]:80 default_server;
    server_name     _;
    root            /var/share/nginx/html;
    index           index.html
}