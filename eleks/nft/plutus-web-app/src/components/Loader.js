import { ReactComponent as Spinner } from '../icons/spinner.svg';
import '../styles/Loader.scss';

const Loader = () => (
  <div className='Loader'>
    <h5 className='heading'>Loading...</h5>
    <Spinner className='spinner' width='100' />
  </div>
);

export default Loader;
