export AWS_DEFAULT_REGION=ap-southeast-2
export BORIS_ENVIRONMENT=ci
export HOST=localhost
export PORT=8888
box -e ci ssh :gateway -- -f -L 8888:boris.ambiata.com:80 -N
