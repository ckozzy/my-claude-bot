#!/usr/bin/env python3
"""
Generate GitHub App JWT and installation access token.
"""

import sys
import time
import json
import argparse
from pathlib import Path

try:
    import jwt
    import requests
except ImportError:
    print("Error: Required packages not installed.", file=sys.stderr)
    print("Install with: pip3 install PyJWT cryptography requests", file=sys.stderr)
    sys.exit(1)


def generate_jwt(app_id, private_key_path):
    """Generate a JWT for GitHub App authentication."""
    private_key = Path(private_key_path).read_text()

    # JWT expires in 10 minutes (maximum allowed by GitHub)
    now = int(time.time())
    payload = {
        'iat': now,
        'exp': now + 600,
        'iss': app_id
    }

    token = jwt.encode(payload, private_key, algorithm='RS256')
    return token


def get_installation_token(app_id, installation_id, private_key_path):
    """Get an installation access token for GitHub App."""
    jwt_token = generate_jwt(app_id, private_key_path)

    url = f"https://api.github.com/app/installations/{installation_id}/access_tokens"
    headers = {
        "Accept": "application/vnd.github+json",
        "Authorization": f"Bearer {jwt_token}",
        "X-GitHub-Api-Version": "2022-11-28"
    }

    response = requests.post(url, headers=headers)
    response.raise_for_status()

    data = response.json()
    return data['token']


def main():
    parser = argparse.ArgumentParser(description='Generate GitHub App tokens')
    parser.add_argument('--app-id', required=True, help='GitHub App ID')
    parser.add_argument('--installation-id', required=True, help='Installation ID')
    parser.add_argument('--private-key', required=True, help='Path to private key file')
    parser.add_argument('--jwt-only', action='store_true', help='Only generate JWT (not installation token)')

    args = parser.parse_args()

    try:
        if args.jwt_only:
            token = generate_jwt(args.app_id, args.private_key)
        else:
            token = get_installation_token(args.app_id, args.installation_id, args.private_key)

        print(token)
    except Exception as e:
        print(f"Error: {e}", file=sys.stderr)
        sys.exit(1)


if __name__ == '__main__':
    main()
