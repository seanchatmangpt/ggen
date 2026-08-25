#!/usr/bin/env python3
import hashlib,json,pathlib,re,sys
root=pathlib.Path(__file__).resolve().parents[1]
subject=json.loads((root/'.ggen/epistemic-replication/r52-subject.json').read_text())
errors=[]
def check(name,ok):
    print(f'{name}={"PASS" if ok else "FAIL"}')
    if not ok: errors.append(name)
check('SCHEMA',subject.get('schema')=='ggen.epistemic-consumer-subject/1')
check('REPO',subject.get('consumer_repo')=='seanchatmangpt/ggen')
check('BASE_SHA',bool(re.fullmatch(r'[0-9a-f]{40}',subject.get('consumer_base',''))))
check('PRODUCER_SHA',bool(re.fullmatch(r'[0-9a-f]{40}',subject.get('producer_base',''))))
check('PACK',subject.get('producer_pack')=='epistemic-sensor-factory-pack')
check('TARGET',subject.get('producer_target_token')=='esf:ggenTarget')
check('NO_DO',subject.get('consequential_do') is False)
check('AUTHORITY','VERIFY' in subject.get('authority','') and 'DO' not in subject.get('authority','').split('|'))
check('STANDING',subject.get('standing')=='ADMITTED')
digest=hashlib.sha256(json.dumps(subject,sort_keys=True,separators=(',',':')).encode()).hexdigest()
print('SUBJECT_DIGEST='+digest)
if errors:
    print('REFUSED[R52_CONSUMER_CONTRACT]='+','.join(errors)); sys.exit(1)
print('R52_CONSUMER_CONTRACT=ALIVE')
