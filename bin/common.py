import json, os, re, shutil, subprocess, sys, tempfile, urllib2

cwd = None
def cd(path):
  global cwd
  print "cd " + path
  cwd = path
def stdout(command):
  if not cwd: raise Exception("uninitialized cwd")
  print command
  result = subprocess.check_output(command, shell=True, stderr=subprocess.STDOUT, cwd=cwd).strip()
  if not result: raise Exception("empty output of " + command)
  return result
def run(command):
  if not cwd: raise Exception("uninitialized cwd")
  print command
  subprocess.check_output(command, shell=True, stderr=subprocess.STDOUT, cwd=cwd)
def call(command):
  if not cwd: raise Exception("uninitialized cwd")
  print command
  sys.stdout.flush()
  return subprocess.call(command, shell=True, stderr=subprocess.STDOUT, cwd=cwd)

def check_exists(path):
  if not os.path.exists(path): raise Exception(path + " does not exist")
  return path

date_format = "%Y-%m-%d %H:%M:%S %z"
def utc(date):
  if sys.platform == "darwin": return stdout("TZ=UTC date -jf '" + date_format + "' '" + date + "' +'" + date_format + "'")
  elif sys.platform.startswith("linux"): return stdout("TZ=UTC date -d '" + date + "' +'" + date_format + "'")
  else: raise Exception("unsupported platform " + sys.platform)

class GithubRef(object):
  def __init__(self, user, repo, sha):
    self.user = user
    self.repo = repo
    self.sha = sha
    self.path = None
  def __str__(self):
    return self.url
  @property
  def markdown(self):
    return "[{}]({})".format(self.sha[:7], self.url)
  @property
  def url(self):
    return "https://github.com/{}/{}/commit/{}".format(self.user, self.repo, self.sha)
  def resolve_remote(self, path):
    if not self.path: raise Exception("uninitialized path")
    if os.path.isabs(path): return self.resolve_remote(os.path.relpath(path, self.path))
    tree_url = "https://github.com/{}/{}/tree/{}".format(self.user, self.repo, self.sha)
    return os.path.join(tree_url, path)
  def download(self, https):
    if self.path: raise Exception("initialized path")
    self.path = tempfile.mkdtemp(prefix = "github_")
    cd(self.path)
    if https: remote = "https://github.com/{}/{}.git".format(self.user, self.repo)
    else: remote = "git@github.com:{}/{}.git".format(self.user, self.repo)
    run("git clone " + remote + " " + self.path)
    run("git checkout " + self.sha)
  def download_https(self):
    return self.download(https=True)
  def download_ssh(self):
    return self.download(https=False)
  def resolve_local(self, path):
    if os.path.isabs(path): raise Exception("absolute path")
    return check_exists(os.path.join(self.path, path))
  def __enter__(self):
    return self
  def __exit__(self, user=None, repo=None, sha=None):
    self.cleanup()
  def cleanup(self):
    if self.path:
      shutil.rmtree(self.path)
      self.path = None

def github_ref(path):
  cd(path)
  root_path = stdout("git rev-parse --show-toplevel")
  cd(root_path)
  sha = stdout("git rev-parse HEAD")
  remote = stdout("git remote -v | grep origin | awk -F' ' '{print $2}' | head -n 1")
  m_url1 = re.match(r"git@github\.com:(.*?)/(.*?).git", remote)
  m_url2 = re.match(r"https://github\.com/(.*?)/(.*?).git", remote)
  m_url = m_url1 or m_url2
  ref = GithubRef(m_url.groups()[0], m_url.groups()[1], sha)
  ref.path = path
  return ref

class GithubApi(object):
  def __init__(self, token):
    self.token = token
  def __call__(self, url, values=None):
    diagnostics = "curl -H \"Authorization: token {}\" ".format(self.token)
    if values: diagnostics += "-X POST -d '{}' ".format(json.dumps(values))
    diagnostics += url
    print diagnostics
    headers = {"Authorization": "token " + self.token}
    data = json.dumps(values) if values else None
    req = urllib2.Request(url, data, headers)
    resp = urllib2.urlopen(req).read()
    return json.loads(resp)
