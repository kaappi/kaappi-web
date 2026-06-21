#!/bin/bash
set -e

KAAPPI="${KAAPPI:-zig-out/bin/kaappi}"
PORT=19877
PASS=0
FAIL=0

check() {
    local name="$1" expected="$2" actual="$3"
    if [ "$expected" = "$actual" ]; then
        PASS=$((PASS + 1))
        echo "  PASS: $name"
    else
        FAIL=$((FAIL + 1))
        echo "  FAIL: $name"
        echo "    expected: $expected"
        echo "    got:      $actual"
    fi
}

DIR="$(cd "$(dirname "$0")" && pwd)"
export DYLD_LIBRARY_PATH="${DYLD_LIBRARY_PATH:+$DYLD_LIBRARY_PATH:}$DIR/../../kaappi-http"

$KAAPPI \
  --lib-path "$DIR/../../kaappi-http/lib" \
  --lib-path "$DIR/../../kaappi-json/lib" \
  --lib-path "$DIR/../lib" \
  "$DIR/test-app-server.scm" &
SERVER_PID=$!
sleep 0.5

cleanup() { kill $SERVER_PID 2>/dev/null; wait $SERVER_PID 2>/dev/null; }
trap cleanup EXIT

echo "=== GET / ==="
check "text response" "Hello, World!" "$(curl -s http://127.0.0.1:$PORT/)"

echo "=== GET /json ==="
check "json response" '{"ok":true}' "$(curl -s http://127.0.0.1:$PORT/json)"

echo "=== GET /users/42 ==="
check "path param" '{"id":42,"name":"Alice"}' "$(curl -s http://127.0.0.1:$PORT/users/42)"

echo "=== GET /users/5/posts/99 ==="
check "multi params" '{"user":5,"post":99}' "$(curl -s http://127.0.0.1:$PORT/users/5/posts/99)"

echo "=== POST /echo ==="
check "json body echo" '{"hello":"world"}' \
  "$(curl -s -X POST -H 'Content-Type: application/json' -d '{"hello":"world"}' http://127.0.0.1:$PORT/echo)"

echo "=== DELETE /items/7 ==="
check "delete" '{"deleted":true}' "$(curl -s -X DELETE http://127.0.0.1:$PORT/items/7)"

echo "=== GET /missing ==="
STATUS=$(curl -s -o /dev/null -w '%{http_code}' http://127.0.0.1:$PORT/missing)
check "404" "404" "$STATUS"

echo ""
echo "=== Results: $PASS passed, $FAIL failed ==="
[ "$FAIL" -eq 0 ]
