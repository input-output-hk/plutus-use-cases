import json
import requests
from pyjsonq import JsonQ
from pygments import highlight, lexers, formatters
import jq

wallet_cid = {}
for i in range(1,5):
    wallet_cid[i] = open(f'tmp/W{i}.cid', 'r').read()

currency_symbol = open('tmp/symbol', 'r').read()

BASE_URL = 'http://localhost:8080/api/'

def json_load(url, http_verb='get', data = []):
    print( f'[{http_verb}] {url}' )
    r = requests.request(
        http_verb, 
        url=url, 
        json=data
    )

    return r.json()

def normalize_json(j):
    if j == []:
        return []
    return JsonQ(data=j).at('.cicCurrentState.observableState').get()

def ppjson(j):
    formatted_json = json.dumps(j, indent=2, sort_keys=True)
    colorful_json = highlight(formatted_json, lexers.JsonLexer(), formatters.TerminalFormatter())
    print(colorful_json)

def get_wallet_status(wallet_num):
    j = json_load(
        f'{BASE_URL}new/contract/instance/{wallet_cid[wallet_num]}/status'
    )
    
    return normalize_json(j)

def get_wallet_funds(wallet_num):
    j = json_load(
        f'{BASE_URL}new/contract/instance/{wallet_cid[wallet_num]}/endpoint/funds', 
        'post'
    )
    
    return normalize_json(j)

for i in range(1, 5):
    print( ppjson(get_wallet_status(i)) )
    print( ppjson(get_wallet_funds(i)) )

