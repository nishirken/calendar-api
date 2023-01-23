const {exec} = require('node:child_process');
const {promisify} = require('node:util');
const {strict: assert} = require('node:assert');

const asyncExec = promisify(exec);

(async () => {
  try {
    const {
      TAG_APP,
      TAG_MIGRATIONS,
      TOKEN,
      COMPOSE_FILE,
      USERNAME,
      HOST,
    } = process.env;

    assert(TAG_APP);
    assert(TAG_MIGRATIONS);
    assert(TOKEN);
    assert(COMPOSE_FILE);
    assert(USERNAME);
    assert(HOST);

    await asyncExec(
      `scp -i ~/.ssh/gandi_key ci/${COMPOSE_FILE} ${USERNAME}@${HOST}:/var/calendar-api/${COMPOSE_FILE}`
    );

    const sshExec = cmd => asyncExec(`ssh -i ~/.ssh/gandi_key ${USERNAME}@${HOST} "${cmd}"`);

    console.log('Logging in...');
    const loginRes = await sshExec(
      `podman login registry.gitlab.com/nishirken/calendar-api --username nishirken --password ${TOKEN}`
    );

    if (loginRes.stdout.toLowerCase().includes('error') || loginRes.stderr) {
      throw new Error(`Login failed\nstdout: ${loginRes.stderr}\nstderr: ${loginRes.stderr}`);
    }

    console.log('Login succeeded', loginRes.stdout);

    console.log('Stopping containers...');
    await sshExec('podman-compose -f /var/calendar-api/${COMPOSE_FILE} down');
    console.log('Stopping containers succeeded');

    console.log('Running containers...');
    const runningRes = await sshExec(
      `TAG_APP=${TAG_APP} TAG_MIGRATIONS=${TAG_MIGRATIONS} podman-compose -f /var/calendar-api/${COMPOSE_FILE} up -d`
    );

    if (runningRes.stdout.match(/exit code.*[1-9]/) !== null) {
      throw new Error(`Running containers failed\nstdout: ${runningRes.stdout}\nstderr: ${runningRes.stderr}`);
    }

    console.log('Running containers succeeded', runningRes.stdout);

    console.log('Checking if webapp is running...');
    const healthCheckRes = await sshExec('curl -S -s http://localhost:8081/healthcheck || exit 1');

    if (healthCheckRes.stderr) {
      throw new Error(`Webapp healthcheck failed\nstdout: ${healthCheckRes.stdout}\nstderr: ${healthCheckRes.stderr}`);
    }

    console.log('Webapp is running');
  } catch (error) {
    console.error(error);

    process.exit(1);
  }
})();

