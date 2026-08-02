#!/usr/bin/env python3
"""Independent verifier for a v26.8.3 PRD/ARD authority bundle.

The verifier never edits the subject and never accepts producer-assigned standing.
"""
from __future__ import annotations
import argparse, hashlib, json, pathlib, re, sys
from typing import Any
import jsonschema

EMBEDDED_SCHEMA={'$id': 'https://chatmangpt.com/schemas/v26.8.3/release-authority.schema.json', '$schema': 'https://json-schema.org/draft/2020-12/schema', 'additionalProperties': False, 'properties': {'base_sha': {'pattern': '^[0-9a-f]{40}$', 'type': 'string'}, 'components': {'items': {'additionalProperties': False, 'properties': {'actuation': {'type': 'boolean'}, 'id': {'pattern': '^[A-Z]+-C-[0-9]{3}$', 'type': 'string'}, 'name': {'type': 'string'}, 'responsibility': {'type': 'string'}}, 'required': ['id', 'name', 'responsibility', 'actuation'], 'type': 'object'}, 'minItems': 5, 'type': 'array'}, 'documents': {'items': {'additionalProperties': False, 'properties': {'kind': {'enum': ['PRD', 'ARD']}, 'path': {'type': 'string'}, 'required_sections': {'items': {'type': 'string'}, 'minItems': 5, 'type': 'array', 'uniqueItems': True}, 'sha256': {'pattern': '^[0-9a-f]{64}$', 'type': 'string'}}, 'required': ['kind', 'path', 'sha256', 'required_sections'], 'type': 'object'}, 'maxItems': 2, 'minItems': 2, 'type': 'array'}, 'forbidden_overclaims': {'items': {'type': 'string'}, 'minItems': 4, 'type': 'array', 'uniqueItems': True}, 'initial_standing': {'const': 'PARTIAL_ALIVE'}, 'interfaces': {'items': {'additionalProperties': False, 'properties': {'authority_transfer': {'type': 'boolean'}, 'contract': {'type': 'string'}, 'direct_actuation': {'const': False}, 'id': {'type': 'string'}, 'receipt_required': {'const': True}, 'source': {'type': 'string'}, 'target': {'type': 'string'}}, 'required': ['id', 'source', 'target', 'contract', 'authority_transfer', 'direct_actuation', 'receipt_required'], 'type': 'object'}, 'minItems': 2, 'type': 'array'}, 'invariants': {'items': {'type': 'string'}, 'minItems': 6, 'type': 'array', 'uniqueItems': True}, 'launch_predicates': {'items': {'type': 'string'}, 'minItems': 8, 'type': 'array', 'uniqueItems': True}, 'peer': {'additionalProperties': False, 'properties': {'base_sha': {'pattern': '^[0-9a-f]{40}$', 'type': 'string'}, 'contract_id': {'const': 'chatman.ggen-ggen-legacy.v26.8.3/1'}, 'repository': {'pattern': '^[^/]+/[^/]+$', 'type': 'string'}}, 'required': ['repository', 'base_sha', 'contract_id'], 'type': 'object'}, 'release': {'const': 'v26.8.3'}, 'repository': {'pattern': '^[^/]+/[^/]+$', 'type': 'string'}, 'requirements': {'items': {'additionalProperties': False, 'properties': {'acceptance': {'minLength': 5, 'type': 'string'}, 'components': {'items': {'type': 'string'}, 'minItems': 1, 'type': 'array', 'uniqueItems': True}, 'id': {'pattern': '^[A-Z]+-FR-[0-9]{3}$', 'type': 'string'}, 'owner': {'minLength': 2, 'type': 'string'}, 'self_certifies': {'const': False}, 'state': {'const': 'ADMITTED_TARGET'}, 'title': {'minLength': 3, 'type': 'string'}, 'verifier': {'minLength': 3, 'type': 'string'}}, 'required': ['id', 'title', 'owner', 'components', 'verifier', 'acceptance', 'state', 'self_certifies'], 'type': 'object'}, 'minItems': 10, 'type': 'array'}, 'role': {'enum': ['REPOSITORY_MANUFACTURING_KERNEL', 'EXECUTABLE_ARCHITECTURE_CORPUS']}, 'schema_version': {'const': 'chatman.v26.8.3.release-authority/1'}, 'standing_ceiling': {'const': 'ALIVE'}}, 'required': ['schema_version', 'release', 'repository', 'base_sha', 'role', 'peer', 'standing_ceiling', 'initial_standing', 'documents', 'requirements', 'components', 'interfaces', 'invariants', 'launch_predicates', 'forbidden_overclaims'], 'title': 'v26.8.3 PRD/ARD Release Authority', 'type': 'object'}
ALLOWED_ROLES={"REPOSITORY_MANUFACTURING_KERNEL","EXECUTABLE_ARCHITECTURE_CORPUS"}
REQUIRED_INVARIANTS={"ZERO_UNRECEIPTED_ACTUATION","NO_SELF_CERTIFICATION","OBSERVATION_IS_NOT_ADMISSION","CHECKPOINT_IS_NOT_CROWN","REPOSITORIES_REALIZE_CAPABILITIES","RELEASE_IS_NOT_SUNSET"}
REQUIRED_LAUNCH={"exact_source_identity","document_digests_match","requirements_traceable","component_owners_complete","verifiers_assigned","self_certification_cycles_zero","direct_actuation_paths_zero","replay_differences_zero","independent_peer_verifier_passed","standing_alive"}
FORBIDDEN_PHRASES=("SOC 2 compliant","certified secure","production proven","zero risk")

def canonical(obj:Any)->bytes:
    return (json.dumps(obj,sort_keys=True,separators=(",",":"),ensure_ascii=False)+"\n").encode()

def digest(path:pathlib.Path)->str: return hashlib.sha256(path.read_bytes()).hexdigest()
def load_json(path:pathlib.Path): return json.loads(path.read_text())

def findings(root:pathlib.Path, expected_repository:str|None=None, expected_role:str|None=None)->list[str]:
    out=[]
    authority_path=root/'authority/v26.8.3/release-authority.json'
    if not authority_path.exists(): return ['AUTHORITY_ABSENT']
    authority=load_json(authority_path)
    try: jsonschema.Draft202012Validator(EMBEDDED_SCHEMA).validate(authority)
    except Exception as e: out.append('JSON_SCHEMA_REFUSED:'+str(e).splitlines()[0])
    if expected_repository and authority.get('repository')!=expected_repository: out.append('REPOSITORY_IDENTITY_MISMATCH')
    if expected_role and authority.get('role')!=expected_role: out.append('ROLE_MISMATCH')
    if authority.get('role') not in ALLOWED_ROLES: out.append('ROLE_UNKNOWN')
    docs=authority.get('documents',[])
    if sorted(d.get('kind') for d in docs)!=['ARD','PRD']: out.append('DOCUMENT_SET_MISMATCH')
    markdown=''
    for d in docs:
        p=root/d['path']
        if not p.exists(): out.append('DOCUMENT_ABSENT:'+d['path']); continue
        text=p.read_text(); markdown+='\n'+text
        if digest(p)!=d.get('sha256'): out.append('DOCUMENT_DIGEST_MISMATCH:'+d['path'])
        for section in d.get('required_sections',[]):
            if not re.search(r'^#+\s+'+re.escape(section)+r'\s*$',text,re.M): out.append('SECTION_ABSENT:'+d['path']+':'+section)
    reqs=authority.get('requirements',[]); ids=[r.get('id') for r in reqs]
    if len(ids)!=len(set(ids)): out.append('DUPLICATE_REQUIREMENT')
    components={c.get('id'):c for c in authority.get('components',[])}
    if len(components)!=len(authority.get('components',[])): out.append('DUPLICATE_COMPONENT')
    for r in reqs:
        rid=r.get('id','')
        if rid not in markdown: out.append('REQUIREMENT_NOT_IN_DOCUMENTS:'+rid)
        if not r.get('owner'): out.append('OWNER_ABSENT:'+rid)
        if not r.get('verifier'): out.append('VERIFIER_ABSENT:'+rid)
        if r.get('self_certifies') is not False: out.append('SELF_CERTIFICATION:'+rid)
        for cid in r.get('components',[]):
            if cid not in components: out.append('UNKNOWN_COMPONENT:'+rid+':'+cid)
    if set(authority.get('invariants',[])) < REQUIRED_INVARIANTS: out.append('INVARIANT_SET_INCOMPLETE')
    if set(authority.get('launch_predicates',[])) < REQUIRED_LAUNCH: out.append('LAUNCH_THEOREM_INCOMPLETE')
    actuators=[c['id'] for c in components.values() if c.get('actuation')]
    role=authority.get('role')
    if role=='EXECUTABLE_ARCHITECTURE_CORPUS' and actuators: out.append('CORPUS_ACTUATION_REFUSED:'+','.join(actuators))
    if role=='REPOSITORY_MANUFACTURING_KERNEL' and actuators!=['GGEN-C-007']: out.append('KERNEL_ACTUATION_TOPOLOGY_INVALID')
    for i in authority.get('interfaces',[]):
        if i.get('direct_actuation') is not False: out.append('DIRECT_ACTUATION:'+i.get('id',''))
        if i.get('receipt_required') is not True: out.append('RECEIPT_NOT_REQUIRED:'+i.get('id',''))
    low=markdown.lower()
    for phrase in FORBIDDEN_PHRASES:
        if phrase.lower() in low: out.append('FORBIDDEN_OVERCLAIM:'+phrase)
    if re.search(r'(?im)^.*aggregate (repository|runtime|production).*\balive\b',markdown): out.append('AGGREGATE_ALIVE_OVERCLAIM')
    if authority.get('initial_standing')!='PARTIAL_ALIVE': out.append('INITIAL_STANDING_INVALID')
    return sorted(set(out))

def verify(root:pathlib.Path, expected_repository=None, expected_role=None):
    authority=load_json(root/'authority/v26.8.3/release-authority.json')
    fs=findings(root,expected_repository,expected_role)
    content_root=hashlib.sha256(b''.join(sorted((p.relative_to(root).as_posix()+':'+digest(p)+'\n').encode() for p in root.rglob('*') if p.is_file() and not any(part in {'.git','target','__pycache__'} for part in p.parts) and p.suffix!='.pyc'))).hexdigest()
    return {'schema_version':'chatman.v26.8.3.prd-ard-verifier/1','subject_repository':authority.get('repository'),'subject_base_sha':authority.get('base_sha'),'subject_role':authority.get('role'),'content_root_sha256':content_root,'requirements':len(authority.get('requirements',[])),'components':len(authority.get('components',[])),'interfaces':len(authority.get('interfaces',[])),'findings':fs,'direct_actuation':False,'self_certification':False,'standing':'ALIVE' if not fs else 'BUILD_BROKEN','claim_ceiling':'PRD_ARD_AUTHORITY_BUNDLE_ONLY'}

def run_mutations(root:pathlib.Path, expected_repository:str, expected_role:str):
    import tempfile, shutil
    cases=[]
    def mutate(name, fn, needle):
        with tempfile.TemporaryDirectory() as td:
            dst=pathlib.Path(td)/'subject'; shutil.copytree(root,dst)
            p=dst/'authority/v26.8.3/release-authority.json'; obj=load_json(p); fn(obj,dst)
            p.write_text(json.dumps(obj,sort_keys=True,separators=(',',':'))+'\n')
            fs=findings(dst,expected_repository,expected_role)
            cases.append({'name':name,'killed':any(needle in x for x in fs),'findings':fs})
    mutate('duplicate requirement',lambda o,_:o['requirements'].append(dict(o['requirements'][0])),'DUPLICATE_REQUIREMENT')
    mutate('unknown component',lambda o,_:o['requirements'][0]['components'].append('NO-C-999'),'UNKNOWN_COMPONENT')
    mutate('missing owner',lambda o,_:o['requirements'][0].__setitem__('owner',''),'OWNER_ABSENT')
    mutate('self certification',lambda o,_:o['requirements'][0].__setitem__('self_certifies',True),'SELF_CERTIFICATION')
    mutate('launch predicate loss',lambda o,_:o['launch_predicates'].pop(),'LAUNCH_THEOREM_INCOMPLETE')
    def tamper(o,r):
        p=r/o['documents'][0]['path']; p.write_text(p.read_text()+'tamper\n')
    mutate('document tamper',tamper,'DOCUMENT_DIGEST_MISMATCH')
    mutate('direct actuation',lambda o,_:o['interfaces'][0].__setitem__('direct_actuation',True),'DIRECT_ACTUATION')
    other='EXECUTABLE_ARCHITECTURE_CORPUS' if expected_role=='REPOSITORY_MANUFACTURING_KERNEL' else 'REPOSITORY_MANUFACTURING_KERNEL'
    mutate('role mismatch',lambda o,_:o.__setitem__('role',other),'ROLE_MISMATCH')
    mutate('undocumented requirement',lambda o,_:o['requirements'][0].__setitem__('id','ZZ-FR-999'),'REQUIREMENT_NOT_IN_DOCUMENTS')
    if expected_role=='REPOSITORY_MANUFACTURING_KERNEL': mutate('non BRCE actuator',lambda o,_:o['components'][0].__setitem__('actuation',True),'KERNEL_ACTUATION_TOPOLOGY_INVALID')
    else: mutate('corpus actuator',lambda o,_:o['components'][0].__setitem__('actuation',True),'CORPUS_ACTUATION_REFUSED')
    return {'cases':cases,'killed':sum(1 for c in cases if c['killed']),'total':len(cases),'standing':'ALIVE' if all(c['killed'] for c in cases) else 'BUILD_BROKEN'}

def main():
    ap=argparse.ArgumentParser(); ap.add_argument('--subject-root',required=True); ap.add_argument('--expected-repository',required=True); ap.add_argument('--expected-role',required=True); ap.add_argument('--output'); ap.add_argument('--self-test',action='store_true')
    a=ap.parse_args(); root=pathlib.Path(a.subject_root); report=verify(root,a.expected_repository,a.expected_role)
    if a.self_test:
        report['mutation_suite']=run_mutations(root,a.expected_repository,a.expected_role)
        if report['mutation_suite']['standing']!='ALIVE': report['standing']='BUILD_BROKEN'
    data=canonical(report)
    if a.output: pathlib.Path(a.output).write_bytes(data)
    sys.stdout.buffer.write(data)
    return 0 if report['standing']=='ALIVE' else 2
if __name__=='__main__': raise SystemExit(main())
